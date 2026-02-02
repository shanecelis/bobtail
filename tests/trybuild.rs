#[test]
fn ui() {
    let t = trybuild::TestCases::new();

    // These tests have stable error messages across feature configurations
    t.compile_fail("tests/ui/warn_*.rs");
    t.compile_fail("tests/ui/define_vis_after_arrow.rs");

    // This test's error message differs with omit-token feature
    // Only run it when the feature is enabled (the .stderr reflects that output)
    #[cfg(feature = "omit-token")]
    t.compile_fail("tests/ui/omit-token/too_many_optionals.rs");

    #[cfg(not(feature = "omit-token"))]
    t.compile_fail("tests/ui/too_many_optionals.rs");

    t.pass("tests/ui/pass_*.rs");
    t.pass("tests/ui/test_manual_macro_export.rs");
    t.pass("tests/ui/pub_macro_export.rs");
    t.pass("tests/ui/pub_macro_no_use.rs");
    t.pass("tests/ui/pub_macro_export_manual.rs");
    t.pass("tests/ui/test_manual_macro_export.rs");
}
