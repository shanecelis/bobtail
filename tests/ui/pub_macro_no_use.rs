mod other_module {
    use bobtail;

    pub struct A(pub u8);

    #[bobtail::block]
    impl A {
        #[bobtail::bob]
        // TODO: I don't understand why reducing visibility makes this work.
        pub(crate) fn b1(&self, a: u8, #[bobtail::tail] b: Option<u8>) -> u8 {
            b.map(|x| x + a + self.0).unwrap_or(a)
        }
    }
}

mod test_module {

    use super::other_module;
    use super::other_module::b1;

    pub(crate) fn test() {
        let a = other_module::A(0);
        // The macro is accessible via `pub(crate) use` from the defining module
        let _ = b1!(a, 1, Some(2));
    }
}


fn main() {
    test_module::test();
}
