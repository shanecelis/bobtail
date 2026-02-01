mod other_module {
    use bobtail;

    pub struct A(pub u8);

    #[bobtail::block]
    impl A {
        #[bobtail::bob]
        pub(crate) fn b(&self, a: u8, #[bobtail::tail] b: Option<u8>) -> u8 {
            b.map(|x| x + a + self.0).unwrap_or(a)
        }
    }

    pub fn run_it() {
        b!(A(0), 1, 2);
    }
}

mod test_module {
    use super::other_module;
    // Import macro from the module
    use super::other_module::b;

    #[test]
    pub fn test() {
        let a = other_module::A(0);
        let _ = b!(a, 1, Some(2));
    }
}

fn main() {
    other_module::run_it();
}
