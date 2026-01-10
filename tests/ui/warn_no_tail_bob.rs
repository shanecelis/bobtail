use bobtail;

// This should emit a warning because there's no #[tail] attribute
// Note: Warnings from proc-macro-error only appear on nightly Rust
#[bobtail::bob]
fn f(a: u8, b: Option<u8>) -> u8 {
  b.map(|x| x + a).unwrap_or(a)
}

fn main() {
    // Use the function to ensure the macro is expanded
    let _ = f(1, Some(2));
    // Also use the macro
    let _ = f!(1, Some(2));
}
