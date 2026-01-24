#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
    t.pass("tests/ui/pass_*.rs");
    t.pass("tests/ui/test_manual_macro_export.rs");
    t.pass("tests/ui/pub_macro_export.rs");
    t.pass("tests/ui/pub_macro_export_manual.rs");
    t.pass("tests/ui/test_manual_macro_export.rs");
}
