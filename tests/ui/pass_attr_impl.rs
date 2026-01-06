use bobtail;

#[derive(Clone, Copy)]
struct PColor(u8);
impl From<u8> for PColor {
    fn from(v: u8) -> Self { PColor(v) }
}

#[derive(Default)]
struct Pico8;

#[bobtail::block]
impl Pico8 {
    #[bobtail::bob]
    fn sset(
        &mut self,
        pos: (u32, u32),
        #[bobtail::tail]
        color: Option<PColor>,
        sheet_index: Option<usize>,
    ) -> Result<(), ()> {
        let _ = (pos, color, sheet_index);
        Ok(())
    }
}

fn main() {
    let mut pico = Pico8::default();
    sset!(pico, (0, 0)).unwrap();
    sset!(pico, (1, 2), PColor(7u8)).unwrap();
    sset!(pico, (3, 4), _, 9usize).unwrap();
}


