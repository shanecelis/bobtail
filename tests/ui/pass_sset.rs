use bobtail::define_tail_optional_macro;

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

define_tail_optional_macro!(
    sset => fn sset(
        &mut self,
        pos: (u32, u32),
        #[tail]
        #[map(PColor::from)]
        color: Option<PColor>,
        sheet_index: Option<usize>,
    ) -> Result<(), ()>;
);

fn main() {
    let mut pico = Pico8;
    let maybe_color: Option<PColor> = None;
    sset!(pico, (0,0), @raw maybe_color, None).unwrap();
}
