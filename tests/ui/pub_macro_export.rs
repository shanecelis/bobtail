
#[macro_use]
mod other_module {
    use bobtail;

    pub struct A(pub u8);

    #[bobtail::block]
    impl A {
        #[bobtail::bob]
        pub fn b1(&self, a: u8, #[bobtail::tail] b: Option<u8>) -> u8 {
            b.map(|x| x + a + self.0).unwrap_or(a)
        }
    }
}

mod test_module {

    use super::other_module;

    fn test() {
        let a = other_module::A(0);
        // This should work if the macro is exported with #[macro_export]
        // The macro should be accessible at crate root when the method is pub
        // For macro-expanded #[macro_export] macros, they are accessible directly without import
        let _ = b1!(a, 1, Some(2));
    }
}

fn main() {}
