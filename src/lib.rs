
#![forbid(unsafe_code)]
//! Tail omittable-parameter macros for methods.
//!
//! Provides:
//! - `tail_define!` to define multiple macros from prototypes in one block.
//!
//! ## Features
//! - Omitted optional args default to `Default::default()`
//! - Provided optional args default to `T::from(expr)` (i.e. `From::from(expr)` with type inferred from the slot)
//! -
//! - Literal `_` is treated like omission (`Default::default()`), so `Option<T>` still becomes `None`.

pub use bobtail_proc::block;
pub use bobtail_proc::bob;

