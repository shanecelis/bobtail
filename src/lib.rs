#![doc(html_root_url = "https://docs.rs/bobtail/0.1.0")]
#![doc = include_str!("../README.md")]
#![forbid(unsafe_code)]
#![forbid(missing_docs)]

// Allows the code to refer to itself as `bobtail` instead of `crate`.
extern crate self as bobtail;

pub use bobtail_proc_macros::block;
pub use bobtail_proc_macros::bob;
pub use bobtail_proc_macros::define;

#[doc(hidden)]
#[macro_export]
macro_rules! __bobtail_handle_underscore {
    // Handle `_` token by converting to Default::default()
    (_) => { ::core::default::Default::default() };
    ($e:expr) => { ::core::convert::From::from($e) };
}
