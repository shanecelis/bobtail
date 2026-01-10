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

#[bobtail::block]
impl Pico8 {
    #[bobtail::bob(sset_macro_one)]
    fn sset(
        &mut self,
        _pos: (u32, u32),
        #[tail] _color: Option<PColor>,
        _sheet_index: Option<usize>,
    ) -> Result<(), ()> {
        self.last.push("sset");
        Ok(())
    }

    #[bobtail::bob]
    fn sspr(&mut self, _pos: (u32, u32), #[tail] _color: Option<PColor>) -> Result<(), ()> {
        self.last.push("sspr");
        Ok(())
    }
}

#[bobtail::bob]
fn prnt(_pos: (u32, u32), #[tail] _color: Option<PColor>, input: MyInput) -> MyInput {
    input
}

// Also test that we can define additional macro proxies explicitly, in one block,
// alongside the attribute-driven ones above.
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
        input: MyInput,
    ) -> MyInput;

    prnt_any => fn prnt(
        pos: (u32, u32),
        color: Option<PColor>,
        input: MyInput,
    ) -> MyInput;
}

#[test]
fn tail_define_individual_method_with_explicit_macro_name() {
    let mut pico = Pico8::default();
    sset_macro_one!(pico, (0, 0)).unwrap();
    sset_macro_one!(pico, (0, 0), PColor(1)).unwrap();
    sset_macro_one!(pico, (0, 0), _, 3usize).unwrap();
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
    assert_eq!(prnt_all!((0, 0), PColor(7), MyInput(9)), MyInput(9));
    assert_eq!(prnt_all!((0, 0), PColor(7), _), MyInput(0));
    assert_eq!(prnt_all!((0, 0), PColor(7), Default::default()), MyInput(0));
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
