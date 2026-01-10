#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
    t.pass("tests/ui/pass_*.rs");
    // Warning tests - these compile but should emit warnings
    t.pass("tests/ui/warn_*.rs");
}
