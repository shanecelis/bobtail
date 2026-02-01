mod other_module {
    use bobtail;

    pub struct A;

    #[bobtail::block]
    impl A {
        #[bobtail::bob]
        pub(crate) fn b(&self, a: u8, #[bobtail::tail] b: Option<u8>) -> u8 {
            b.map(|x| x + a).unwrap_or(a)
        }
    }
}

// Import the macro from the module where it's defined (pub(crate) use)
use other_module::b1;

fn main() {
    let a = other_module::A;
    // Test that the macro is available via `pub(crate) use`
    let _ = b!(a, 1, Some(2));
}
