use bobtail;

#[bobtail::bob]
fn f(a: u8, b: Option<u8>) -> u8 {
  b.map(|x| x + a).unwrap_or(a)
}

fn main() {
    // ERROR: no #[tail] attribute is present, so all arguments are required.
    // This should fail to compile.
    assert_eq!(f!(1), 1);
}
