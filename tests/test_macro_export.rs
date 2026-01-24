use bobtail;

mod other_module {
    use bobtail;

    pub struct A;

    #[bobtail::block]
    impl A {
        #[bobtail::bob]
        pub fn b(&self, a: u8, #[bobtail::tail] b: Option<u8>) -> u8 {
            b.map(|x| x + a).unwrap_or(a)
        }
    }
}

fn main() {
    let a = other_module::A;
    // Test if the macro is exported - should work if #[macro_export] is present
    let _ = b!(a, 1, Some(2));
}
