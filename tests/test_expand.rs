// #![feature(trace_macros)]
use bobtail;

#[bobtail::bob]
fn f(a: u8, b: Option<u8>) -> u8 {
    b.map(|x| x + a).unwrap_or(a)
}

#[test]
fn a() {
    // trace_macros!(true);
    let _x = f!(1, Some(2));
    // trace_macros!(false);
}
