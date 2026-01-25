#[derive(Clone, Copy)]
struct PColor(u8);
impl From<u8> for PColor {
    fn from(v: u8) -> Self { PColor(v) }
}

struct Pico8;
impl Pico8 {
    fn sset(&mut self, _pos: (u32,u32), _color: Option<PColor>, _sheet_index: Option<usize>) -> Result<(), ()> {
        Ok(())
    }
}

bobtail::define! {
    sset => fn sset(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
        sheet_index: Option<usize>,
    ) -> Result<(), ()>;
}

fn main() {
    let mut pico = Pico8;
    let maybe_color: Option<PColor> = None;
    // Note: `_` placeholder requires the `omit-token` feature
    sset!(pico, (0,0), maybe_color, None).unwrap();
}
