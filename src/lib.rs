
#![forbid(unsafe_code)]
//! Tail omittable-parameter macros for methods.
//!
//! Provides:
//! - `define_tail_optional_macro!` to generate a free macro that calls a method and fills trailing omittable params.
//! - `tail_define!` to define multiple macros from prototypes in one block.
//!
//! ## Features
//! - M required args, N optional trailing args
//! - Omitted optional args default to `Default::default()`
//! - Provided optional args default to `T::from(expr)` (i.e. `From::from(expr)` with type inferred from the slot)
//! - Literal `None` is treated like omission (`Default::default()`), so `Option<T>` still becomes `None`
//! - `@raw expr` escape hatch passes `expr` through unchanged (useful for passing a pre-built `Option<T>`, etc.)
//! - Per-optional-arg pre-conversion hook: `name: Ty => path::to::conv` uses `T::from(conv(expr))`

pub use bobtail_proc::block;
pub use bobtail_proc::bob;

mod tail_define;

#[doc(hidden)]
#[macro_export]
macro_rules! __tail_omittable_default_for {
    (( $name:ident : $ty:ty => $conv:path )) => { ::core::default::Default::default() };
    (( $name:ident : $ty:ty )) => { ::core::default::Default::default() };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __tail_omittable_wrap_expr {
    ($e:expr, ( $name:ident : $ty:ty => $conv:path )) => {
        ::core::convert::From::from($conv($e))
    };
    ($e:expr, ( $name:ident : $ty:ty )) => {
        ::core::convert::From::from($e)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __tail_omittable_munch {
    // Skip any leading commas in the provided stream (handles trailing commas too).
    (@munch
        $kind:ident,
        ($($callee:tt)*),
        ($($req:expr),*),
        (, $($tail:tt)*),
        ( $($slot_spec:tt),* ),
        ( $($acc:expr),* )
    ) => {
        $crate::__tail_omittable_munch!(@munch
            $kind,
            ($($callee)*),
            ($($req),*),
            ( $($tail)* ),
            ( $($slot_spec),* ),
            ( $($acc),* )
        )
    };

    // Literal `None` for the next slot is treated like omission/default.
    (@munch
        $kind:ident,
        ($($callee:tt)*),
        ($($req:expr),*),
        ( None $(, $($tail:tt)*)? ),
        ( $slot_spec:tt $(, $slots:tt)* ),
        ( $($acc:expr),* )
    ) => {
        $crate::__tail_omittable_munch!(@munch
            $kind,
            ($($callee)*),
            ($($req),*),
            ( $($($tail)*)? ),
            ( $($slots),* ),
            ( $($acc,)* $crate::__tail_omittable_default_for!($slot_spec) )
        )
    };

    // `@raw expr` provided for the next slot: passthrough unchanged.
    (@munch
        $kind:ident,
        ($($callee:tt)*),
        ($($req:expr),*),
        ( @raw $e:expr $(, $($tail:tt)*)? ),
        ( $slot_spec:tt $(, $slots:tt)* ),
        ( $($acc:expr),* )
    ) => {
        $crate::__tail_omittable_munch!(@munch
            $kind,
            ($($callee)*),
            ($($req),*),
            ( $($($tail)*)? ),
            ( $($slots),* ),
            ( $($acc,)* $e )
        )
    };

    // `@raw` must be followed by an expression.
    (@munch
        $kind:ident,
        ($($callee:tt)*),
        ($($req:expr),*),
        ( @raw , $($tail:tt)* ),
        ( $($slot_spec:tt),* ),
        ( $($acc:expr),* )
    ) => {
        compile_error!("@raw must be followed by an expression");
    };
    (@munch
        $kind:ident,
        ($($callee:tt)*),
        ($($req:expr),*),
        ( @raw ),
        ( $($slot_spec:tt),* ),
        ( $($acc:expr),* )
    ) => {
        compile_error!("@raw must be followed by an expression");
    };

    // Regular `expr` provided for the next slot.
    (@munch
        $kind:ident,
        ($($callee:tt)*),
        ($($req:expr),*),
        ( $e:expr $(, $($tail:tt)*)? ),
        ( $slot_spec:tt $(, $slots:tt)* ),
        ( $($acc:expr),* )
    ) => {
        $crate::__tail_omittable_munch!(@munch
            $kind,
            ($($callee)*),
            ($($req),*),
            ( $($($tail)*)? ),
            ( $($slots),* ),
            ( $($acc,)* $crate::__tail_omittable_wrap_expr!($e, $slot_spec) )
        )
    };

    // No provided args left: default remaining slots.
    (@munch
        $kind:ident,
        ($($callee:tt)*),
        ($($req:expr),*),
        ( $(,)* ),
        ( $slot_spec:tt $(, $slots:tt)* ),
        ( $($acc:expr),* )
    ) => {
        $crate::__tail_omittable_munch!(@munch
            $kind,
            ($($callee)*),
            ($($req),*),
            (),
            ( $($slots),* ),
            ( $($acc,)* $crate::__tail_omittable_default_for!($slot_spec) )
        )
    };

    // Done.
    (@munch
        $kind:ident,
        ($($callee:tt)*),
        ($($req:expr),*),
        ( $(,)* ),
        (),
        ( $($acc:expr),* )
    ) => {
        $crate::__tail_omittable_munch!(@finish
            $kind,
            ($($callee)*),
            ($($req),*),
            ($($acc),*)
        )
    };

    // Too many provided optionals.
    (@munch
        $kind:ident,
        ($($callee:tt)*),
        ($($req:expr),*),
        ( $($extra:tt)+ ),
        (),
        ( $($acc:expr),* )
    ) => {
        compile_error!("too many optional arguments");
    };

    (@finish
        method,
        ($obj:expr, $method:ident),
        ($($req:expr),*),
        ($($acc:expr),*)
    ) => {
        $obj.$method($($req),*, $($acc),*)
    };

    (@finish
        fn,
        ($func:path),
        ($($req:expr),*),
        ($($acc:expr),*)
    ) => {
        $func($($req),*, $($acc),*)
    };
}

/// Define a free macro that calls a method and supplies trailing "omittable" args.
///
/// Omittable argument type must implement:
/// - `Default` (used when omitted)
/// - `From<Expr>` for each expression form you want to pass (used when provided)
///
/// Syntax:
/// ```rust,ignore
/// define_tail_optional_macro!(
///     macro_name => fn method_name(
///         &mut self,
///         req1: Ty1,
///         req2: Ty2,
///         #[tail]      // first tail-omittable parameter
///         opt1: OmittableTy1,
///         #[map(path::to::conv)] // optional conversion for an omittable param
///         opt2: OmittableTy2,
///     ) -> Result<(), ()>; // return type is optional and ignored
/// );
/// ```
#[macro_export]
macro_rules! define_tail_optional_macro {
    (
        $(#[$meta:meta])*
        $mac_name:ident => fn $method:ident ( $($params:tt)* ) $(-> $ret:ty)? ;
    ) => {
        $crate::define_tail_optional_macro!(@proto
            $(#[$meta])*
            $mac_name => $method
            ( $($params)* )
        );
    };

    // --- Prototype parsing helpers (implementation detail) ---
    (@proto
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($params:tt)* )
    ) => {
        $crate::define_tail_optional_macro!(@proto_req
            $(#[$meta])*
            $mac_name => $method
            ( ) // required names
            ( ) // optional specs as tt: (name: ty) or (name: ty => conv)
            $($params)*
        );
    };

    (@proto_finish
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_name:ident ,)* )
        ( $($opt_spec:tt ,)* )
    ) => {
        $crate::define_tail_optional_macro!(@define_generated_macro
            $(#[$meta])*
            $mac_name => $method
            ( $($req_name),* )
            ( $($opt_spec),* )
        );
    };

    // Skip leading commas.
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_req
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* )
            $($rest)*
        );
    };

    // End of parameter list.
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
    ) => {
        $crate::define_tail_optional_macro!(@proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* )
        );
    };

    // Receiver forms to ignore (do not count as required args).
    (@proto_req $(#[$meta:meta])* $mac_name:ident => $method:ident ( $($req_acc:ident ,)* ) ( $($opt_acc:tt ,)* ) & mut self $($rest:tt)* ) => {
        $crate::define_tail_optional_macro!(@proto_req $(#[$meta])* $mac_name => $method ( $($req_acc,)* ) ( $($opt_acc,)* ) $($rest)* );
    };
    (@proto_req $(#[$meta:meta])* $mac_name:ident => $method:ident ( $($req_acc:ident ,)* ) ( $($opt_acc:tt ,)* ) & self $($rest:tt)* ) => {
        $crate::define_tail_optional_macro!(@proto_req $(#[$meta])* $mac_name => $method ( $($req_acc,)* ) ( $($opt_acc,)* ) $($rest)* );
    };
    (@proto_req $(#[$meta:meta])* $mac_name:ident => $method:ident ( $($req_acc:ident ,)* ) ( $($opt_acc:tt ,)* ) mut self $($rest:tt)* ) => {
        $crate::define_tail_optional_macro!(@proto_req $(#[$meta])* $mac_name => $method ( $($req_acc,)* ) ( $($opt_acc,)* ) $($rest)* );
    };
    (@proto_req $(#[$meta:meta])* $mac_name:ident => $method:ident ( $($req_acc:ident ,)* ) ( $($opt_acc:tt ,)* ) self $($rest:tt)* ) => {
        $crate::define_tail_optional_macro!(@proto_req $(#[$meta])* $mac_name => $method ( $($req_acc,)* ) ( $($opt_acc,)* ) $($rest)* );
    };

    // Start tail: `#[tail]` (optionally followed by `#[map(...)]`)
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        #[tail] #[map($conv:path)] $(#[$_other:meta])*
        mut $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty => $conv ), )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        #[tail] #[map($conv:path)] $(#[$_other:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty => $conv ), )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        #[tail] $(#[$_other:meta])*
        mut $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty ), )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        #[tail] $(#[$_other:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty ), )
            $($rest)*
        );
    };

    // Required param: just record name (type comes from method signature at call site).
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        $(#[$_attr:meta])*
        mut $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_req
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* $name, )
            ( $($opt_acc,)* )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_req
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* $name, )
            ( $($opt_acc,)* )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        $(#[$_attr:meta])*
        mut $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(@proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* $name, )
            ( $($opt_acc,)* )
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(@proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* $name, )
            ( $($opt_acc,)* )
        );
    };

    // Optional mode: consume remaining params as tail-omittable.
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
    ) => {
        $crate::define_tail_optional_macro!(@proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* )
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        #[map($conv:path)] $(#[$_other:meta])*
        mut $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty => $conv ), )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        #[map($conv:path)] $(#[$_other:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty => $conv ), )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        $(#[$_attr:meta])*
        mut $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty ), )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(@proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty ), )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        #[map($conv:path)] $(#[$_other:meta])*
        mut $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(@proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty => $conv ), )
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        #[map($conv:path)] $(#[$_other:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(@proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty => $conv ), )
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        $(#[$_attr:meta])*
        mut $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(@proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty ), )
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:ident ,)* )
        ( $($opt_acc:tt ,)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(@proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc,)* )
            ( $($opt_acc,)* ( $name : $ty ), )
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
        $crate::__tail_omittable_munch!(@munch
            method,
            ($obj, $method),
            ($($req),*),
            ( $($provided)* ),
            ( $($opt_specs),* ),
            ()
        )
    };
}

