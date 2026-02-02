mod other_module {
    use bobtail;

    pub struct A(pub u8);

    #[bobtail::block]
    impl A {
        // Use pub(crate) visibility for the macro to avoid #[macro_export]
        #[bobtail::bob(pub(crate) b2)]
        pub fn b(&self, a: u8, #[bobtail::tail] b: Option<u8>) -> u8 {
            b.map(|x| x + a + self.0).unwrap_or(a)
        }
    }
}

mod test_module {

    use super::other_module;
    // pub(crate) generates `pub(crate) use b2;` in the defining module
    use super::other_module::b2;

    pub(crate) fn test() {
        let a = other_module::A(0);
        // The macro is accessible via `pub(crate) use` from the defining module
        let _ = b2!(a, 1, Some(2));
    }
}


fn main() {
    test_module::test();
}
