
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
    (@munch
        $kind:ident,
        ($($callee:tt)*),
        ($($req:expr),*),
        ( (None) $(, $($tail:tt)*)? ),
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
    // --- New syntax: method-like function prototype ---
    //
    // Example:
    // define_tail_optional_macro!(
    //     sset => fn sset(
    //         &mut self,
    //         pos: (u32, u32),
    //         #[tail]
    //         color: Option<PColor>,
    //         sheet_index: Option<usize>,
    //     ) -> Result<(), ()>;
    // );
    (
        $(#[$meta:meta])*
        $mac_name:ident => fn $method:ident ( $($params:tt)* ) $(-> $ret:ty)? ;
    ) => {
        $crate::define_tail_optional_macro!(
            @proto
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
        $crate::define_tail_optional_macro!(
            @proto_req
            $(#[$meta])*
            $mac_name => $method
            ( )
            ( )
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
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_req
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* )
            $($rest)*
        );
    };

    // End of parameter list.
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* )
        );
    };

    // Receiver forms to ignore (do not count as required args).
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        & mut self $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_req
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        & self $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_req
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        mut self $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_req
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        self $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_req
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* )
            $($rest)*
        );
    };

    // Start of tail-omittables: the first parameter preceded by `#[tail]` is the first omittable,
    // and all remaining parameters are treated as tail-omittable.
    //
    // Conversions use `#[map(path::to::conv)]` and can be combined with `#[tail]`.
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] #[map($conv:path)] $(#[$_other:meta])*
        mut $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty => $conv ), )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] #[map($conv:path)] $(#[$_other:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty => $conv ), )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] #[map($conv:path)] $(#[$_other:meta])*
        mut $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty => $conv ), )
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] #[map($conv:path)] $(#[$_other:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty => $conv ), )
        );
    };

    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] $(#[$_other:meta])*
        mut $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] $(#[$_other:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] $(#[$_other:meta])*
        mut $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] $(#[$_other:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
        );
    };

    // Required parameter (ignores any non-tail attrs).
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        $(#[$_attr:meta])*
        mut $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_req
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* $name, )
            ( $($opt_acc)* )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_req
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* $name, )
            ( $($opt_acc)* )
            $($rest)*
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        $(#[$_attr:meta])*
        mut $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* $name, )
            ( $($opt_acc)* )
        );
    };
    (@proto_req
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* $name, )
            ( $($opt_acc)* )
        );
    };

    // Optional mode: consume remaining params as tail-omittable.
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* )
        );
    };
    // In optional mode, allow per-parameter `#[tail]` / `#[map(path)]` too (useful for conversion hooks).
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[map($conv:path)] $(#[$_other:meta])*
        mut $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty => $conv ), )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[map($conv:path)] $(#[$_other:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty => $conv ), )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] $(#[$_other:meta])*
        mut $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] $(#[$_other:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[map($conv:path)] $(#[$_other:meta])*
        mut $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty => $conv ), )
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[map($conv:path)] $(#[$_other:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty => $conv ), )
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] $(#[$_other:meta])*
        mut $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        #[tail] $(#[$_other:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        $(#[$_attr:meta])*
        mut $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_opt
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
            $($rest)*
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        $(#[$_attr:meta])*
        mut $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
        );
    };
    (@proto_opt
        $(#[$meta:meta])*
        $mac_name:ident => $method:ident
        ( $($req_acc:tt)* )
        ( $($opt_acc:tt)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::define_tail_optional_macro!(
            @proto_finish
            $(#[$meta])*
            $mac_name => $method
            ( $($req_acc)* )
            ( $($opt_acc)* ( $name : $ty ), )
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

/// Define a free macro that calls a free function (or associated function path) and supplies trailing omittable args.
///
/// This is the same idea as [`define_tail_optional_macro!`], but there is **no receiver** argument.
///
/// Syntax:
/// ```rust,ignore
/// define_tail_optional_fn_macro!(
///     macro_name => path::to::function;
///     ( req1: Ty1, req2: Ty2, ... )
///     [ opt1: OmittableTy1, opt2: OmittableTy2 => path::to::conv, ... ]
/// );
/// ```
#[macro_export]
macro_rules! define_tail_optional_fn_macro {
    (
        $(#[$meta:meta])*
        $mac_name:ident => $func:path ;
        ( $($req_name:ident : $req_ty:ty),* $(,)? )
        [ $($opt_name:ident : $opt_ty:ty $(=> $opt_conv:path)?),* $(,)? ]
    ) => {
        $crate::define_tail_optional_fn_macro!(@define_generated_macro
            $(#[$meta])*
            $mac_name => ($func)
            ( $($req_name),* )
            ( $( ( $opt_name : $opt_ty $(=> $opt_conv)? ) ),* )
        );
    };

    (@define_generated_macro
        $(#[$meta:meta])*
        $mac_name:ident => ($func:path)
        ( $($req_name:ident),* )
        ( $($opt_specs:tt),* )
    ) => {
        macro_rules! __define_tail_optional_fn_macro_with_dollar {
            ($d:tt) => {
                $(#[$meta])*
                #[macro_export]
                macro_rules! $mac_name {
                    ($($d $req_name:expr),*) => {{
                        $crate::define_tail_optional_fn_macro!(@call
                            $func,
                            ( $( $d $req_name ),* ),
                            (),
                            ( $($opt_specs),* )
                        )
                    }};
                    ($($d $req_name:expr),*,) => {{
                        $crate::define_tail_optional_fn_macro!(@call
                            $func,
                            ( $( $d $req_name ),* ),
                            (),
                            ( $($opt_specs),* )
                        )
                    }};
                    ($($d $req_name:expr),*, $d( $d provided:tt )+ ) => {{
                        $crate::define_tail_optional_fn_macro!(@call
                            $func,
                            ( $( $d $req_name ),* ),
                            ( $d( $d provided )+ ),
                            ( $($opt_specs),* )
                        )
                    }};
                    ($($d $req_name:expr),*, $d( $d provided:tt )+ ,) => {{
                        $crate::define_tail_optional_fn_macro!(@call
                            $func,
                            ( $( $d $req_name ),* ),
                            ( $d( $d provided )+ , ),
                            ( $($opt_specs),* )
                        )
                    }};
                }
            };
        }

        __define_tail_optional_fn_macro_with_dollar!($);
    };

    (@call
        $func:path,
        ($($req:expr),*),
        ($($provided:tt)*),
        ( $($opt_specs:tt),* )
    ) => {
        $crate::__tail_omittable_munch!(@munch
            fn,
            ($func),
            ($($req),*),
            ( $($provided)* ),
            ( $($opt_specs),* ),
            ()
        )
    };
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
        sset => fn sset(
            &mut self,
            pos: (u32, u32),
            #[tail]
            #[map(PColor::from)]
            color: Option<PColor>,
            sheet_index: Option<usize>,
        ) -> Result<(), ()>;
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

    fn free_sset(
        pos: (u32, u32),
        color: Option<PColor>,
        sheet_index: Option<usize>,
    ) -> (Option<(u32, u32)>, Option<Option<PColor>>, Option<Option<usize>>) {
        (Some(pos), Some(color), Some(sheet_index))
    }

    define_tail_optional_fn_macro!(
        free_sset_m => crate::tests::free_sset;
        (pos: (u32, u32))
        [color: Option<PColor> => PColor::from, sheet_index: Option<usize>]
    );

    #[test]
    fn free_function_macro_works() {
        let (last_pos, last_color, last_sheet) = free_sset_m!((9, 9), 7u8, 3usize);
        assert_eq!(last_pos, Some((9, 9)));
        assert_eq!(last_color, Some(Some(PColor(7))));
        assert_eq!(last_sheet, Some(Some(3)));

        let (_pos, color, sheet) = free_sset_m!((1, 1), None, None);
        assert_eq!(color, Some(None));
        assert_eq!(sheet, Some(None));
    }
}
