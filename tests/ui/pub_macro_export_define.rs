mod other_module {
    use bobtail;

    pub struct A;

    impl A {
        pub fn b(&self, a: u8, #[bobtail::tail] b: Option<u8>) -> u8 {
            b.map(|x| x + a).unwrap_or(a)
        }
    }
    bobtail::define! {
        #[macro_export]
        fn b(&self, a: u8, #[bobtail::tail] b: Option<u8>) -> u8;
    }
}

mod test_module {

    use super::other_module;
    use crate::b;

    fn test() {
        let a = other_module::A;
        // This should work if the macro is exported with #[macro_export]
        // The macro should be accessible at crate root when the method is pub
        // For macro-expanded #[macro_export] macros, they are accessible directly without import
        let _ = b!(a, 1, Some(2));
    }
}

fn main() {}
