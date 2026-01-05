use tail_optional_macros as tail_omittable; // enables #[tail_omittable::block]
use tail_optional_macros::tail_omittable; // enables #[tail_omittable]

#[derive(Clone, Copy)]
struct PColor(u8);
impl From<u8> for PColor {
    fn from(v: u8) -> Self { PColor(v) }
}

#[derive(Default)]
struct Pico8;

#[tail_omittable::block]
impl Pico8 {
    #[tail_omittable(conv(color(PColor::from)))]
    fn sset(
        &mut self,
        pos: (u32, u32),
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
    sset!(pico, (1, 2), 7u8).unwrap();
    sset!(pico, (3, 4), None, 9usize).unwrap();
}


