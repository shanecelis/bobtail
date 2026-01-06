use bobtail::define_tail;

#[derive(Clone, Copy)]
struct PColor(u8);
impl From<u8> for PColor {
    fn from(v: u8) -> Self { PColor(v) }
}

struct Pico8;
impl Pico8 {
    fn sset(&mut self, _pos: (u32, u32), _color: Option<PColor>, _sheet_index: Option<usize>) -> Result<(), ()> {
        Ok(())
    }
}

define_tail!(
    sset /* new macro name */ => fn sset(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
        sheet_index: Option<usize>,
    ) -> Result<(), ()>;
);

fn main() {
    let mut pico = Pico8;
    sset!(pico, (0, 0)).unwrap();
    sset!(pico, (1, 2), PColor(7u8)).unwrap();
    sset!(pico, (3, 4), None, 9usize).unwrap();
}


