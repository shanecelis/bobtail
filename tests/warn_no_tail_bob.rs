use bobtail;

// This should emit a warning because there's no #[tail] attribute
#[bobtail::bob]
fn test_func(a: u8, b: u8) -> u8 {
    a + b
}

#[test]
fn test_warning_for_bob() {
    // This test just needs to compile to trigger the warning
    // The warning should appear during compilation
    let _ = test_func(1, 2);
    let _ = test_func!(1, 2); // Use the macro to ensure it's generated
}
