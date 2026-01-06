use bobtail::tail_define;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PColor(u8);

#[derive(Default)]
struct Pico8 {
    last: Vec<&'static str>,
}

impl Pico8 {
    fn sset(&mut self, _pos: (u32, u32), _color: Option<PColor>, _sheet_index: Option<usize>) -> Result<(), ()> {
        self.last.push("sset");
        Ok(())
    }

    fn sspr(&mut self, _pos: (u32, u32), _color: Option<PColor>) -> Result<(), ()> {
        self.last.push("sspr");
        Ok(())
    }
}

fn prnt(_pos: (u32, u32), _color: Option<PColor>) -> &'static str {
    "prnt"
}

// Define macros individually (separate invocations)
tail_define!(
    sset_macro_one => fn sset(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
        sheet_index: Option<usize>,
    ) -> Result<(), ()>;
);

tail_define!(
    // implicit macro name == function name
    fn sspr(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
    ) -> Result<(), ()>;
);

tail_define!(
    // implicit macro name == function name
    fn prnt(
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
    );
);

// Define macros all together (single invocation) with distinct macro names to avoid redefinitions
tail_define!(
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
    );
);

#[test]
fn tail_define_individual_method_with_explicit_macro_name() {
    let mut pico = Pico8::default();
    sset_macro_one!(pico, (0, 0)).unwrap();
    sset_macro_one!(pico, (0, 0), PColor(1)).unwrap();
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
    assert_eq!(prnt!((0, 0)), "prnt");
    assert_eq!(prnt!((0, 0), PColor(7)), "prnt");
}

#[test]
fn tail_define_all_together() {
    let mut pico = Pico8::default();
    sset_macro_all!(pico, (0, 0)).unwrap();
    sspr_all!(pico, (1, 2)).unwrap();
    assert_eq!(prnt_all!((9, 9)), "prnt");
    assert_eq!(pico.last, vec!["sset", "sspr"]);
}


