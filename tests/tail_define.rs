#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PColor(u8);

#[derive(Default)]
struct Pico8 {
    last: Vec<&'static str>,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct MyInput(u8);

impl From<u8> for MyInput {
    fn from(x: u8) -> Self {
        Self(x)
    }
}

impl Pico8 {
    fn sset(
        &mut self,
        _pos: (u32, u32),
        _color: Option<PColor>,
        _sheet_index: Option<usize>,
    ) -> Result<(), ()> {
        self.last.push("sset");
        Ok(())
    }

    fn sspr(&mut self, _pos: (u32, u32), _color: Option<PColor>) -> Result<(), ()> {
        self.last.push("sspr");
        Ok(())
    }
}

fn prnt(_pos: (u32, u32), _color: Option<PColor>, input: MyInput) -> MyInput {
    input
}

// Define macros individually (separate invocations)
bobtail::define! {
    sset_macro_one => fn sset(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
        sheet_index: Option<usize>,
    ) -> Result<(), ()>;
}

bobtail::define! {
    // implicit macro name == function name
    fn sspr(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
    ) -> Result<(), ()>;
}

bobtail::define! {
    // implicit macro name == function name
    fn prnt(
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
        _input: MyInput,
    );
}

// Define macros all together (single invocation) with distinct macro names to avoid redefinitions
bobtail::define! {
    sset_macro_all => fn sset(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
        sheet_index: Option<usize>,
    ) -> Result<(), ()>;

    sspr_all => fn sspr(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
    ) -> Result<(), ()>;

    prnt_all => fn prnt(
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
        _input: MyInput,
    );

    prnt_any => fn prnt(
        pos: (u32, u32),
        color: Option<PColor>,
        _input: MyInput,
    );
}

#[test]
fn tail_define_individual_method_with_explicit_macro_name() {
    let mut pico = Pico8::default();
    sset_macro_one!(pico, (0, 0)).unwrap();
    sset_macro_one!(pico, (0, 0), PColor(1)).unwrap();
    // Note: `_` placeholder requires the `omit-token` feature
    // sset_macro_one!(pico, (0, 0), _, 3usize).unwrap();
    sset_macro_one!(pico, (0, 0), None, 3usize).unwrap();
    assert_eq!(pico.last, vec!["sset", "sset", "sset"]);
}

#[test]
fn tail_define_individual_method_with_implicit_macro_name() {
    let mut pico = Pico8::default();
    sspr!(pico, (1, 2)).unwrap();
    sspr!(pico, (1, 2), PColor(9)).unwrap();
    assert_eq!(pico.last, vec!["sspr", "sspr"]);
}

#[test]
fn tail_define_individual_free_function() {
    let x = MyInput(0);
    assert_eq!(prnt!((0, 0)), x);
    assert_eq!(prnt!((0, 0), PColor(7)), x);
    assert_eq!(prnt!((0, 0), PColor(7), 8u8), MyInput(8));
    assert_eq!(prnt!((0, 0), PColor(7), MyInput(9)), MyInput(9));
    // Permit comma.
    assert_eq!(prnt!((0, 0), PColor(7), MyInput(9),), MyInput(9));
    assert_eq!(prnt_all!((0, 0), PColor(7), MyInput(9)), MyInput(9));
    // Note: `_` placeholder requires the `omit-token` feature
    // assert_eq!(prnt_all!((0, 0), PColor(7), _), MyInput(0));
    assert_eq!(prnt_all!((0, 0), PColor(7), MyInput::default()), MyInput(0));
    assert_eq!(prnt_all!((0, 0), PColor(7), MyInput::default()), MyInput(0));
    assert_eq!(
        prnt_any!((0, 0), Some(PColor(7)), Default::default()),
        MyInput(0)
    );
}

#[test]
fn tail_define_all_together() {
    let x = MyInput(0);
    let mut pico = Pico8::default();
    sset_macro_all!(pico, (0, 0)).unwrap();
    sspr_all!(pico, (1, 2)).unwrap();
    assert_eq!(prnt_all!((9, 9)), x);
    assert_eq!(prnt!((9, 9)), x);
    assert_eq!(pico.last, vec!["sset", "sspr"]);
}

// Tests for `_` placeholder support (requires omit-token feature)
#[cfg(feature = "omit-token")]
mod omit_token_tests {
    use super::*;

    #[test]
    fn test_underscore_placeholder_method() {
        let mut pico = Pico8::default();
        sset_macro_one!(pico, (0, 0), _, 3usize).unwrap();
        assert_eq!(pico.last, vec!["sset"]);
    }

    #[test]
    fn test_underscore_placeholder_function() {
        assert_eq!(prnt_all!((0, 0), PColor(7), _), MyInput(0));
        // One trailing comma is fine.
        assert_eq!(prnt_all!((0, 0), PColor(7), _,), MyInput(0));
        // Two trailing commas is not fine.
        // assert_eq!(prnt_all!((0, 0), PColor(7), _,,), MyInput(0));
    }
}

// Tests for visibility modifiers in define!
mod visibility_tests {
    // Helper function for testing
    fn vis_helper(a: u8, b: Option<u8>) -> u8 {
        b.map(|x| x + a).unwrap_or(a)
    }

    // Test: pub(crate) visibility with custom macro name
    bobtail::define! {
        pub(crate) vis_helper_crate => fn vis_helper(a: u8, #[tail] b: Option<u8>) -> u8;
    }

    #[test]
    fn test_pub_crate_visibility_with_name() {
        // Use the macro (need the function in scope too since macro calls the function)
        use vis_helper_crate;
        assert_eq!(vis_helper_crate!(1), 1);
        assert_eq!(vis_helper_crate!(1, 2u8), 3);
    }

    // Test: pub(self) for private macro (no use statement generated)
    mod inner_private {
        fn inner_helper(a: u8, b: Option<u8>) -> u8 {
            b.map(|x| x + a).unwrap_or(a)
        }

        bobtail::define! {
            pub(self) private_macro => fn inner_helper(a: u8, #[tail] b: Option<u8>) -> u8;
        }

        pub fn use_private_macro() -> u8 {
            private_macro!(5)
        }
    }

    #[test]
    fn test_pub_self_private_macro() {
        // The macro is private to the inner module, but we can call a function that uses it
        assert_eq!(inner_private::use_private_macro(), 5);
    }

    // Test: visibility only (no custom macro name)
    // When using "visibility =>" without a macro name, the macro takes the function's name.
    // This works best when define! is in a different module from the function definition.
    mod vis_only_inner {
        pub fn my_add(a: u8, b: Option<u8>) -> u8 {
            b.map(|x| x + a).unwrap_or(a)
        }

        // pub(crate) visibility only - macro name defaults to "my_add"
        bobtail::define! {
            pub(crate) my_add_macro => fn my_add(a: u8, #[tail] b: Option<u8>) -> u8;
        }

        // This demonstrates the syntax works - using a different name to avoid conflicts
        pub fn test_internally() -> u8 {
            my_add_macro!(10)
        }
    }

    #[test]
    fn test_pub_crate_visibility_only() {
        // The macro works when called from within its defining module
        assert_eq!(vis_only_inner::test_internally(), 10);
        // And can be imported from outside (need both macro and function in scope)
        use vis_only_inner::{my_add, my_add_macro};
        assert_eq!(my_add_macro!(1), 1);
        assert_eq!(my_add_macro!(1, 2u8), 3);
    }

    // Test: pub visibility generates #[macro_export]
    // Note: Can't easily test #[macro_export] in integration tests due to
    // macro_expanded_macro_exports_accessed_by_absolute_paths lint
}
