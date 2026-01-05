
#![forbid(unsafe_code)]
//! Tail omittable-parameter macros for methods.
//!
//! Provides:
//! - `__opt!` and `__opt_conv!` legacy helpers for `None`, `Some(...)`, and `@raw ...` passthrough
//! - `define_tail_optional_macro!` to generate a free macro that calls a method and fills trailing omittable params.
//!
//! ## Features
//! - M required args, N optional trailing args
//! - Omitted optional args default to `Default::default()`
//! - Provided optional args default to `T::from(expr)` (i.e. `From::from(expr)` with type inferred from the slot)
//! - Literal `None` is treated like omission (`Default::default()`), so `Option<T>` still becomes `None`
//! - `@raw expr` escape hatch passes `expr` through unchanged (useful for passing a pre-built `Option<T>`, etc.)
//! - Per-optional-arg pre-conversion hook: `name: Ty => path::to::conv` uses `T::from(conv(expr))`

/// Wrap an expression into `Some(...)`, with support for `None` and `@raw` passthrough.
///
/// - `__opt!(None)` => `None`
/// - `__opt!(@raw expr)` => `expr`
/// - `__opt!(x)` => `Some(x)`
#[macro_export]
macro_rules! __opt {
    (@raw $e:expr) => { $e };
    (None) => { None };
    ((None)) => { None };
    ($e:expr) => { Some($e) };
}

/// Like [`__opt!`], but applies a conversion function/path to wrapped values.
///
/// - `__opt_conv!(x, PColor::from)` => `Some(PColor::from(x))`
/// - `__opt_conv!(None, ...)` => `None`
/// - `__opt_conv!(@raw opt, ...)` => `opt`
#[macro_export]
macro_rules! __opt_conv {
    (@raw $e:expr, $conv:path) => { $e };
    (None, $conv:path) => { None };
    ((None), $conv:path) => { None };
    ($e:expr, $conv:path) => { Some($conv($e)) };
}

/// Define a free macro that calls a method and supplies trailing "omittable" args.
///
/// An omittable argument type must implement:
/// - `Default` (used when omitted)
/// - `From<Expr>` for each expression form you want to pass (used when provided)
///
/// `Option<T>` is a natural fit:
/// - `Option<T>::default()` is `None`
/// - `Option<T>::from(x)` is `Some(x)`
///
/// Syntax:
/// ```rust,ignore
/// define_tail_optional_macro!(
///     macro_name => method_name
///     ( req1: Ty1, req2: Ty2, ... )
///     [ opt1: OmittableTy1, opt2: OmittableTy2 => path::to::conv, ... ]
/// );
/// ```
///
/// Generated call form:
/// ```rust,ignore
/// macro_name!(obj, req1, req2, ...);                      // pads omitted with Default::default()
/// macro_name!(obj, req1, req2, ..., x);                   // uses T::from(x)
/// macro_name!(obj, req1, req2, ..., None, y);             // `None` means "use default" (works nicely with Option)
/// macro_name!(obj, req1, req2, ..., @raw already_built);   // passes through unchanged
/// ```
#[macro_export]
macro_rules! define_tail_optional_macro {
    (
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_name:ident : $req_ty:ty),* $(,)? )
        [ $($opt_name:ident : $opt_ty:ty $(=> $opt_conv:path)?),* $(,)? ]
    ) => {
        $crate::define_tail_optional_macro!(@define_generated_macro
            $(#[$meta])*
            $mac_name => $method
            ( $($req_name),* )
            ( $( ( $opt_name : $opt_ty $(=> $opt_conv)? ) ),* )
        );
    };

    (@define_generated_macro
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_name:ident),* )
        ( $($opt_specs:tt),* )
    ) => {
        macro_rules! __define_tail_optional_macro_with_dollar {
            ($d:tt) => {
                $(#[$meta])*
                #[macro_export]
                macro_rules! $mac_name {
                    // Exactly the required args (no optionals).
                    ($d obj:expr $(, $d $req_name:expr )* ) => {{
                        $crate::define_tail_optional_macro!(@call
                            $d obj, $method,
                            ( $( $d $req_name ),* ),
                            (),
                            ( $($opt_specs),* )
                        )
                    }};
                    ($d obj:expr $(, $d $req_name:expr )* ,) => {{
                        $crate::define_tail_optional_macro!(@call
                            $d obj, $method,
                            ( $( $d $req_name ),* ),
                            (),
                            ( $($opt_specs),* )
                        )
                    }};

                    // Required args + optional tail args.
                    ($d obj:expr $(, $d $req_name:expr )* , $d( $d provided:tt )+ ) => {{
                        $crate::define_tail_optional_macro!(@call
                            $d obj, $method,
                            ( $( $d $req_name ),* ),
                            ( $d( $d provided )+ ),
                            ( $($opt_specs),* )
                        )
                    }};
                    ($d obj:expr $(, $d $req_name:expr )* , $d( $d provided:tt )+ ,) => {{
                        $crate::define_tail_optional_macro!(@call
                            $d obj, $method,
                            ( $( $d $req_name ),* ),
                            ( $d( $d provided )+ , ),
                            ( $($opt_specs),* )
                        )
                    }};
                }
            };
        }

        __define_tail_optional_macro_with_dollar!($);
    };

    (@call
        $obj:expr,
        $method:ident,
        ($($req:expr),*),
        ($($provided:tt)*),
        ( $($opt_specs:tt),* )
    ) => {
        $crate::define_tail_optional_macro!(@munch
            $obj,
            $method,
            ($($req),*),
            ( $($provided)* ),
            ( $($opt_specs),* ),
            ()
        )
    };

    // Skip any leading commas in the provided stream (handles trailing commas too).
    (@munch
        $obj:expr, $method:ident,
        ($($req:expr),*),
        (, $($tail:tt)*),
        ( $($slot_spec:tt),* ),
        ( $($acc:expr),* )
    ) => {
        $crate::define_tail_optional_macro!(@munch
            $obj, $method,
            ($($req),*),
            ( $($tail)* ),
            ( $($slot_spec),* ),
            ( $($acc),* )
        )
    };

    // Literal `None` for the next slot is treated like omission.
    // This must be handled here (token-level), because once it's captured as an
    // `expr` fragment it becomes opaque and can't be pattern-matched as `None`.
    (@munch
        $obj:expr, $method:ident,
        ($($req:expr),*),
        ( None $(, $($tail:tt)*)? ),
        ( $slot_spec:tt $(, $slots:tt)* ),
        ( $($acc:expr),* )
    ) => {
        $crate::define_tail_optional_macro!(@munch
            $obj, $method,
            ($($req),*),
            ( $($($tail)*)? ),
            ( $($slots),* ),
            ( $($acc,)* $crate::define_tail_optional_macro!(@default_for $slot_spec) )
        )
    };
    (@munch
        $obj:expr, $method:ident,
        ($($req:expr),*),
        ( (None) $(, $($tail:tt)*)? ),
        ( $slot_spec:tt $(, $slots:tt)* ),
        ( $($acc:expr),* )
    ) => {
        $crate::define_tail_optional_macro!(@munch
            $obj, $method,
            ($($req),*),
            ( $($($tail)*)? ),
            ( $($slots),* ),
            ( $($acc,)* $crate::define_tail_optional_macro!(@default_for $slot_spec) )
        )
    };

    // `@raw expr` provided for the next slot.
    (@munch
        $obj:expr, $method:ident,
        ($($req:expr),*),
        ( @raw $e:expr $(, $($tail:tt)*)? ),
        ( $slot_spec:tt $(, $slots:tt)* ),
        ( $($acc:expr),* )
    ) => {
        $crate::define_tail_optional_macro!(@munch
            $obj, $method,
            ($($req),*),
            ( $($($tail)*)? ),
            ( $($slots),* ),
            ( $($acc,)* $crate::define_tail_optional_macro!(@wrap_raw $e, $slot_spec) )
        )
    };

    // `@raw` must be followed by an expression.
    (@munch
        $obj:expr, $method:ident,
        ($($req:expr),*),
        ( @raw , $($tail:tt)* ),
        ( $($slot_spec:tt),* ),
        ( $($acc:expr),* )
    ) => {
        compile_error!("@raw must be followed by an expression");
    };
    (@munch
        $obj:expr, $method:ident,
        ($($req:expr),*),
        ( @raw ),
        ( $($slot_spec:tt),* ),
        ( $($acc:expr),* )
    ) => {
        compile_error!("@raw must be followed by an expression");
    };

    // Regular `expr` provided for the next slot.
    (@munch
        $obj:expr, $method:ident,
        ($($req:expr),*),
        ( $e:expr $(, $($tail:tt)*)? ),
        ( $slot_spec:tt $(, $slots:tt)* ),
        ( $($acc:expr),* )
    ) => {
        $crate::define_tail_optional_macro!(@munch
            $obj, $method,
            ($($req),*),
            ( $($($tail)*)? ),
            ( $($slots),* ),
            ( $($acc,)* $crate::define_tail_optional_macro!(@wrap_expr $e, $slot_spec) )
        )
    };

    // No provided args left: default remaining slots.
    (@munch
        $obj:expr, $method:ident,
        ($($req:expr),*),
        ( $(,)* ),
        ( $slot_spec:tt $(, $slots:tt)* ),
        ( $($acc:expr),* )
    ) => {
        $crate::define_tail_optional_macro!(@munch
            $obj, $method,
            ($($req),*),
            (),
            ( $($slots),* ),
            ( $($acc,)* $crate::define_tail_optional_macro!(@none_for $slot_spec) )
        )
    };

    // Done.
    (@munch
        $obj:expr, $method:ident,
        ($($req:expr),*),
        ( $(,)* ),
        (),
        ( $($acc:expr),* )
    ) => {
        $obj.$method($($req),*, $($acc),*)
    };

    // Too many provided optionals.
    (@munch
        $obj:expr, $method:ident,
        ($($req:expr),*),
        ( $($extra:tt)+ ),
        (),
        ( $($acc:expr),* )
    ) => {
        compile_error!("too many optional arguments");
    };

    (@wrap_expr $e:expr, ( $name:ident : $ty:ty => $conv:path )) => {
        ::core::convert::From::from($conv($e))
    };
    (@wrap_expr $e:expr, ( $name:ident : $ty:ty )) => {
        ::core::convert::From::from($e)
    };

    (@wrap_raw $e:expr, ( $name:ident : $ty:ty => $conv:path )) => {
        $e
    };
    (@wrap_raw $e:expr, ( $name:ident : $ty:ty )) => {
        $e
    };

    (@default_for ( $name:ident : $ty:ty => $conv:path )) => { ::core::default::Default::default() };
    (@default_for ( $name:ident : $ty:ty )) => { ::core::default::Default::default() };

    (@none_for $($t:tt)*) => { $crate::define_tail_optional_macro!(@default_for $($t)*) };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct PColor(u8);
    impl From<u8> for PColor {
        fn from(v: u8) -> Self { PColor(v) }
    }

    #[derive(Default)]
    struct Pico8 {
        last_pos: Option<(u32, u32)>,
        last_color: Option<Option<PColor>>,
        last_sheet_index: Option<Option<usize>>,
    }

    impl Pico8 {
        fn sset(
            &mut self,
            pos: (u32, u32),
            color: Option<PColor>,
            sheet_index: Option<usize>,
        ) -> Result<(), ()> {
            self.last_pos = Some(pos);
            self.last_color = Some(color);
            self.last_sheet_index = Some(sheet_index);
            Ok(())
        }
    }

    define_tail_optional_macro!(
        sset => sset
        (pos: (u32, u32))
        [color: Option<PColor> => PColor::from, sheet_index: Option<usize>]
    );

    #[test]
    fn pads_missing_optionals_with_none() {
        let mut pico = Pico8::default();
        sset!(pico, (1, 2)).unwrap();
        assert_eq!(pico.last_pos, Some((1, 2)));
        assert_eq!(pico.last_color, Some(None));
        assert_eq!(pico.last_sheet_index, Some(None));
    }

    #[test]
    fn wraps_value_in_some_and_applies_conversion_when_specified() {
        let mut pico = Pico8::default();
        sset!(pico, (3, 4), 7u8).unwrap();
        assert_eq!(pico.last_color, Some(Some(PColor(7))));
        assert_eq!(pico.last_sheet_index, Some(None));
    }

    #[test]
    fn literal_none_passes_through_and_subsequent_optionals_wrap() {
        let mut pico = Pico8::default();
        sset!(pico, (5, 6), None, 99usize).unwrap();
        assert_eq!(pico.last_color, Some(None));
        assert_eq!(pico.last_sheet_index, Some(Some(99)));
    }

    #[test]
    fn raw_escape_hatch_passes_expression_through_unchanged() {
        let mut pico = Pico8::default();
        let maybe_color: Option<PColor> = Some(PColor(42));
        let maybe_sheet: Option<usize> = None;

        sset!(pico, (7, 8), @raw maybe_color, @raw maybe_sheet).unwrap();
        assert_eq!(pico.last_color, Some(Some(PColor(42))));
        assert_eq!(pico.last_sheet_index, Some(None));
    }
}
