// Manually define a macro with #[macro_export] at crate root
#[macro_export]
macro_rules! example_macro {
    () => {
        42u8
    };
}

mod test_module {
    pub(crate) fn test() {
        // This should work if #[macro_export] makes the macro available at crate root
        let _ = example_macro!();
    }
}

fn main() {
    // This should also work
    let _ = example_macro!();
    test_module::test()
}
