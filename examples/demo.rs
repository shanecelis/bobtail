#[derive(Clone, Copy, Debug)]
struct PColor(u8);
impl From<u8> for PColor {
    fn from(v: u8) -> Self { PColor(v) }
}

#[derive(Default)]
struct Pico8;

impl Pico8 {
    pub fn sset(
        &mut self,
        pos: (u32, u32),
        color: Option<PColor>,
        sheet_index: Option<usize>,
    ) -> Result<(), ()> {
        println!("sset pos={pos:?} color={color:?} sheet_index={sheet_index:?}");
        Ok(())
    }
}

bobtail::define! {
    /// Set a pixel with optional color and optional sheet index.
    sset => fn sset(
        &mut self,
        pos: (u32, u32),
        #[tail]
        color: Option<PColor>,
        sheet_index: Option<usize>,
    ) -> Result<(), ()>;
}

fn main() -> Result<(), ()> {
    let mut pico = Pico8::default();

    sset!(pico, (0, 0))?;
    sset!(pico, (1, 2), PColor::from(3u8))?;
    sset!(pico, (3, 4), _, 7usize)?;

    let maybe_color: Option<PColor> = Some(PColor(9));
    sset!(pico, (5, 6), maybe_color, _)?;

    Ok(())
}
