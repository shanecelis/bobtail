mod other_module {
    // use bobtail;

    pub struct A;

    /* #[bobtail::block] */
    impl A {
        /* #[bob] */
        pub fn b(&self, /* #[tail] */ a: Option<u8>) -> u8 {
            a.map(|x| x + 1).unwrap_or(0)
        }
    }

    #[macro_export]
    macro_rules! b {
        ($a:expr) => {
            $a.b(None)
        };
        ($a:expr, $b:expr) => {
            $a.b(Some($b))
        };
    }

}

mod test_module {

    use super::other_module;
    use crate::b;
    // #[macro_use]
    // extern crate other_module;

    fn test() {
        let a = other_module::A;
        // This should work if the macro is exported with #[macro_export]
        // Currently this will fail because the macro is not exported
        // The macro should be accessible at crate root when the method is pub
        // let _ = other_module::b!(a, 1, Some(2));
        let _ = b!(a, 1);
    }
}

fn main() {}
