#![doc(html_root_url = "https://docs.rs/bobtail/0.3.0")]
#![doc = include_str!("../README.md")]
#![forbid(unsafe_code)]
#![forbid(missing_docs)]

// Allows the code to refer to itself as `bobtail` instead of `crate`.
extern crate self as bobtail;

pub use bobtail_proc_macros::block;
pub use bobtail_proc_macros::bob;
pub use bobtail_proc_macros::define;

/// Helper macro for processing arguments with `_` placeholder support.
/// This is only available when the `omit-token` feature is enabled.
/// 
/// The macro recursively processes tail arguments, handling `_` as a placeholder
/// for `Default::default()` and converting other expressions via `From::from()`.
#[cfg(feature = "omit-token")]
#[doc(hidden)]
#[macro_export]
macro_rules! __bobtail_munch {
    // ========== FREE FUNCTIONS ==========
    
    // Base case: no more user input, append remaining defaults and call
    (fn $fn:path; [$($args:tt)*]; [$($defaults:tt)*];) => {
        $fn($($args)* $($defaults)*)
    };
    
    // Process `_` placeholder with more user input following
    (fn $fn:path; [$($args:tt)*]; [$_d:expr, $($ds:tt)*]; _, $($rest:tt)+) => {
        $crate::__bobtail_munch!(fn $fn; [$($args)* ::core::default::Default::default(),]; [$($ds)*]; $($rest)+)
    };
    // Process `_` placeholder as last user input (with remaining defaults)
    (fn $fn:path; [$($args:tt)*]; [$_d:expr, $($ds:tt)*]; _ $(,)?) => {
        $crate::__bobtail_munch!(fn $fn; [$($args)* ::core::default::Default::default(),]; [$($ds)*];)
    };
    // Process `_` placeholder as last user input (no remaining defaults)
    (fn $fn:path; [$($args:tt)*]; [$_d:expr]; _ $(,)?) => {
        $crate::__bobtail_munch!(fn $fn; [$($args)* ::core::default::Default::default(),]; [];)
    };
    
    // Process expression with more user input following
    (fn $fn:path; [$($args:tt)*]; [$_d:expr, $($ds:tt)*]; $arg:expr, $($rest:tt)+) => {
        $crate::__bobtail_munch!(fn $fn; [$($args)* ::core::convert::From::from($arg),]; [$($ds)*]; $($rest)+)
    };
    // Process expression as last user input (with remaining defaults)
    (fn $fn:path; [$($args:tt)*]; [$_d:expr, $($ds:tt)*]; $arg:expr $(,)?) => {
        $crate::__bobtail_munch!(fn $fn; [$($args)* ::core::convert::From::from($arg),]; [$($ds)*];)
    };
    // Process expression as last user input (no remaining defaults)
    (fn $fn:path; [$($args:tt)*]; [$_d:expr]; $arg:expr $(,)?) => {
        $crate::__bobtail_munch!(fn $fn; [$($args)* ::core::convert::From::from($arg),]; [];)
    };
    
    // ========== METHODS ==========
    
    // Base case: no more user input, append remaining defaults and call method
    (method $self:expr, $method:ident; [$($args:tt)*]; [$($defaults:tt)*];) => {
        $self.$method($($args)* $($defaults)*)
    };
    
    // Process `_` placeholder with more user input following
    (method $self:expr, $method:ident; [$($args:tt)*]; [$_d:expr, $($ds:tt)*]; _, $($rest:tt)+) => {
        $crate::__bobtail_munch!(method $self, $method; [$($args)* ::core::default::Default::default(),]; [$($ds)*]; $($rest)+)
    };
    // Process `_` placeholder as last user input (with remaining defaults)
    (method $self:expr, $method:ident; [$($args:tt)*]; [$_d:expr, $($ds:tt)*]; _ $(,)?) => {
        $crate::__bobtail_munch!(method $self, $method; [$($args)* ::core::default::Default::default(),]; [$($ds)*];)
    };
    // Process `_` placeholder as last user input (no remaining defaults)
    (method $self:expr, $method:ident; [$($args:tt)*]; [$_d:expr]; _ $(,)?) => {
        $crate::__bobtail_munch!(method $self, $method; [$($args)* ::core::default::Default::default(),]; [];)
    };
    
    // Process expression with more user input following
    (method $self:expr, $method:ident; [$($args:tt)*]; [$_d:expr, $($ds:tt)*]; $arg:expr, $($rest:tt)+) => {
        $crate::__bobtail_munch!(method $self, $method; [$($args)* ::core::convert::From::from($arg),]; [$($ds)*]; $($rest)+)
    };
    // Process expression as last user input (with remaining defaults)
    (method $self:expr, $method:ident; [$($args:tt)*]; [$_d:expr, $($ds:tt)*]; $arg:expr $(,)?) => {
        $crate::__bobtail_munch!(method $self, $method; [$($args)* ::core::convert::From::from($arg),]; [$($ds)*];)
    };
    // Process expression as last user input (no remaining defaults)
    (method $self:expr, $method:ident; [$($args:tt)*]; [$_d:expr]; $arg:expr $(,)?) => {
        $crate::__bobtail_munch!(method $self, $method; [$($args)* ::core::convert::From::from($arg),]; [];)
    };
}
