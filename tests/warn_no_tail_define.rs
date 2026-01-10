use bobtail;

fn test_func(a: u8, b: u8) -> u8 {
    a + b
}

// This should emit a warning because there's no #[tail] attribute
bobtail::define! {
    fn test_func(a: u8, b: u8) -> u8;
}

#[test]
fn test_warning_for_define() {
    // This test just needs to compile to trigger the warning
    let _ = test_func(1, 2);
}
