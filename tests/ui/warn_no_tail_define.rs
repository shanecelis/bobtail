use bobtail;

fn f(a: u8, b: Option<u8>) -> u8 {
  b.map(|x| x + a).unwrap_or(a)
}

// This should emit a warning because there's no #[tail] attribute
// Note: Warnings from proc-macro-error only appear on nightly Rust
bobtail::define! {
    fn f(a: u8, b: Option<u8>) -> u8;
}

fn main() {
    // Use the function
    let _ = f(1, Some(2));
    // Also use the macro
    let _ = f!(1, Some(2));
    // Force a compile failure.
    let _ = f!(1);
}
