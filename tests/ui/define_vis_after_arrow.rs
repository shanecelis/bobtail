// Test that visibility after => is rejected with a helpful error message

fn my_fn(a: u8, b: Option<u8>) -> u8 {
    b.map(|x| x + a).unwrap_or(a)
}

// Invalid: visibility should be before macro name, not before `fn`
bobtail::define! {
    my_macro => pub(crate) fn my_fn(a: u8, #[tail] b: Option<u8>) -> u8;
}

fn main() {
    let _ = my_macro!(1);
}
