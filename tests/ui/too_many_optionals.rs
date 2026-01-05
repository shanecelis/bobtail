use tail_optional_macros::define_tail_optional_macro;

struct Pico8;
impl Pico8 {
    fn sset(&mut self, _pos: (u32,u32), _color: Option<u8>, _sheet_index: Option<usize>) -> Result<(), ()> {
        Ok(())
    }
}

define_tail_optional_macro!(
    sset => fn sset(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<u8>,
        sheet_index: Option<usize>,
    ) -> Result<(), ()>;
);

fn main() {
    let mut pico = Pico8;
    // ERROR: only 2 optional args exist; providing 3 should fail to match.
    sset!(pico, (0,0), 1u8, 2usize, 3usize).unwrap();
}
