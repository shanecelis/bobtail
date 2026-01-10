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
macro_rules! __bobtail_munch {
    // Tail-argument conversion:
    // "_" => Default
    // expr => From::from(expr)
    (@conv _) => { ::core::default::Default::default() };
    (@conv $e:expr) => { ::core::convert::From::from($e) };

    // Parse receiver call: first arg is receiver expression.
    (
        $fn_name:ident,
        receiver,
        ($($pre:ident,)*),
        ($($tail:ident,)*);
        $self_:expr $(, $($rest:tt)*)?
    ) => {
        $crate::__bobtail_munch!(@munch_req
            $fn_name,
            receiver,
            $self_,
            ($($pre,)*),
            ($($tail,)*),
            (),
            $($($rest)*)?
        )
    };

    // Parse non-receiver call: args are the rest.
    (
        $fn_name:ident,
        no_receiver,
        ($($pre:ident,)*),
        ($($tail:ident,)*);
        $($rest:tt)*
    ) => {
        $crate::__bobtail_munch!(@munch_req
            $fn_name,
            no_receiver,
            (),
            ($($pre,)*),
            ($($tail,)*),
            (),
            $($rest)*
        )
    };

    // ---- Munch required args (must provide all pre args) ----
    (@munch_req
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        (),
        ($($tail:ident,)*),
        ($($req_acc:expr,)*),
        $($rest:tt)*
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($tail,)*),
            ($($req_acc,)*),
            (),
            $($rest)*
        )
    };

    (@munch_req
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($need:ident, $($need_rest:ident,)*),
        ($($tail:ident,)*),
        ($($req_acc:expr,)*),
        $e:expr , $($rest:tt)*
    ) => {
        $crate::__bobtail_munch!(@munch_req
            $fn_name,
            $recv,
            $self_,
            ($($need_rest,)*),
            ($($tail,)*),
            ($($req_acc,)* $e,),
            $($rest)*
        )
    };

    (@munch_req
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($need:ident, $($need_rest:ident,)*),
        ($($tail:ident,)*),
        ($($req_acc:expr,)*),
        $e:expr $(,)?
    ) => {
        $crate::__bobtail_munch!(@munch_req
            $fn_name,
            $recv,
            $self_,
            ($($need_rest,)*),
            ($($tail,)*),
            ($($req_acc,)* $e,),
        )
    };

    // ---- Munch tail args (0..=N provided); `_` means default; missing means default ----
    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        (),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        $(,)?
    ) => {
        $crate::__bobtail_munch!(@call
            $fn_name,
            $recv,
            $self_,
            ($($req_acc,)*),
            ($($tail_acc,)*)
        )
    };

    // Too many arguments: tail list empty but still have tokens.
    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        (),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        $($extra:tt)+
    ) => {
        compile_error!("too many optional arguments");
    };

    // No more provided args: default the remaining tail parameters.
    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($next_tail:ident, $($rest_tail:ident,)*),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        $(,)?
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($rest_tail,)*),
            ($($req_acc,)*),
            ($($tail_acc,)* ::core::default::Default::default(),),
        )
    };

    // Provided `_` for next tail arg.
    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($next_tail:ident, $($rest_tail:ident,)*),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        _ , $($rest:tt)*
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($rest_tail,)*),
            ($($req_acc,)*),
            ($($tail_acc,)* ::core::default::Default::default(),),
            $($rest)*
        )
    };

    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($next_tail:ident, $($rest_tail:ident,)*),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        _ $(,)?
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($rest_tail,)*),
            ($($req_acc,)*),
            ($($tail_acc,)* ::core::default::Default::default(),),
        )
    };

    // Provided `Default::default()` should be passed through directly to let the callee's
    // parameter type drive inference (avoids `From::from(Default::default())` ambiguity).
    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($next_tail:ident, $($rest_tail:ident,)*),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        Default::default() , $($rest:tt)*
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($rest_tail,)*),
            ($($req_acc,)*),
            ($($tail_acc,)* ::core::default::Default::default(),),
            $($rest)*
        )
    };

    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($next_tail:ident, $($rest_tail:ident,)*),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        Default::default() $(,)?
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($rest_tail,)*),
            ($($req_acc,)*),
            ($($tail_acc,)* ::core::default::Default::default(),),
        )
    };

    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($next_tail:ident, $($rest_tail:ident,)*),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        ::core::default::Default::default() , $($rest:tt)*
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($rest_tail,)*),
            ($($req_acc,)*),
            ($($tail_acc,)* ::core::default::Default::default(),),
            $($rest)*
        )
    };

    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($next_tail:ident, $($rest_tail:ident,)*),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        ::core::default::Default::default() $(,)?
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($rest_tail,)*),
            ($($req_acc,)*),
            ($($tail_acc,)* ::core::default::Default::default(),),
        )
    };

    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($next_tail:ident, $($rest_tail:ident,)*),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        ::std::default::Default::default() , $($rest:tt)*
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($rest_tail,)*),
            ($($req_acc,)*),
            ($($tail_acc,)* ::core::default::Default::default(),),
            $($rest)*
        )
    };

    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($next_tail:ident, $($rest_tail:ident,)*),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        ::std::default::Default::default() $(,)?
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($rest_tail,)*),
            ($($req_acc,)*),
            ($($tail_acc,)* ::core::default::Default::default(),),
        )
    };

    // Provided expression for next tail arg.
    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($next_tail:ident, $($rest_tail:ident,)*),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        $e:expr , $($rest:tt)*
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($rest_tail,)*),
            ($($req_acc,)*),
            ($($tail_acc,)* $crate::__bobtail_munch!(@conv $e),),
            $($rest)*
        )
    };

    (@munch_tail
        $fn_name:ident,
        $recv:ident,
        $self_:tt,
        ($next_tail:ident, $($rest_tail:ident,)*),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*),
        $e:expr $(,)?
    ) => {
        $crate::__bobtail_munch!(@munch_tail
            $fn_name,
            $recv,
            $self_,
            ($($rest_tail,)*),
            ($($req_acc,)*),
            ($($tail_acc,)* $crate::__bobtail_munch!(@conv $e),),
        )
    };

    // ---- Final call emission ----
    (@call
        $fn_name:ident,
        no_receiver,
        (),
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*)
    ) => {
        $fn_name($($req_acc,)* $($tail_acc,)*)
    };

    (@call
        $fn_name:ident,
        receiver,
        $self_:expr,
        ($($req_acc:expr,)*),
        ($($tail_acc:expr,)*)
    ) => {
        $self_.$fn_name($($req_acc,)* $($tail_acc,)*)
    };
}
