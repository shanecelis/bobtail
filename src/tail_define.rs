/// Defines one or more bobtail macros from function prototypes.
///
/// Supports:
/// - `macro_name => fn method_name(&mut self, ...) ...;`  (method macro that takes a receiver expr first)
/// - `fn method_name(&mut self, ...) ...;`               (macro name defaults to method name)
/// - `macro_name => fn free_fn_name(...) ...;`           (free function macro)
/// - `fn free_fn_name(...) ...;`                         (macro name defaults to function name)
///
/// Tail begins at the first parameter marked `#[tail]`.
/// Per-parameter conversion is `#[map(path::to::conv)]`.
#[macro_export]
macro_rules! tail_define {
    (@items) => {};

    // item with explicit macro name
    (@items
        $mac:ident => fn $name:ident ( $($params:tt)* ) $(-> $ret:ty)? ;
        $($rest:tt)*
    ) => {
        $crate::tail_define!(@one $mac => fn $name ( $($params)* ) $(-> $ret)? ;);
        $crate::tail_define!(@items $($rest)*);
    };

    // item with implicit macro name (same as function name)
    (@items
        fn $name:ident ( $($params:tt)* ) $(-> $ret:ty)? ;
        $($rest:tt)*
    ) => {
        $crate::tail_define!(@one $name => fn $name ( $($params)* ) $(-> $ret)? ;);
        $crate::tail_define!(@items $($rest)*);
    };

    // Dispatch: method receiver forms
    (@one $mac:ident => fn $name:ident ( & mut self $(, $($rest:tt)*)? ) $(-> $ret:ty)? ;) => {
        $crate::define_tail_optional_macro!(
            $mac => fn $name( &mut self $(, $($rest)*)? ) $(-> $ret)? ;
        );
    };
    (@one $mac:ident => fn $name:ident ( & self $(, $($rest:tt)*)? ) $(-> $ret:ty)? ;) => {
        $crate::define_tail_optional_macro!(
            $mac => fn $name( &self $(, $($rest)*)? ) $(-> $ret)? ;
        );
    };
    (@one $mac:ident => fn $name:ident ( mut self $(, $($rest:tt)*)? ) $(-> $ret:ty)? ;) => {
        $crate::define_tail_optional_macro!(
            $mac => fn $name( mut self $(, $($rest)*)? ) $(-> $ret)? ;
        );
    };
    (@one $mac:ident => fn $name:ident ( self $(, $($rest:tt)*)? ) $(-> $ret:ty)? ;) => {
        $crate::define_tail_optional_macro!(
            $mac => fn $name( self $(, $($rest)*)? ) $(-> $ret)? ;
        );
    };

    // Dispatch: otherwise treat as free function
    (@one $mac:ident => fn $name:ident ( $($params:tt)* ) $(-> $ret:ty)? ;) => {
        $crate::tail_define!(@define_fn_macro $mac => $name ( $($params)* ) );
    };

    // --- free function prototype parser (no receiver) ---
    (@define_fn_macro $mac:ident => $func:ident ( $($params:tt)* ) ) => {
        $crate::tail_define!(@fn_proto $mac => $func ( $($params)* ));
    };

    (@fn_proto $mac:ident => $func:ident ( $($params:tt)* )) => {
        $crate::tail_define!(@fn_req $mac => $func ( ) ( ) $($params)* );
    };

    // skip commas
    (@fn_req $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* ) , $($rest:tt)* ) => {
        $crate::tail_define!(@fn_req $mac => $func ( $($req,)* ) ( $($opt,)* ) $($rest)* );
    };

    // end
    (@fn_req $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* )) => {
        $crate::tail_define!(@fn_finish $mac => $func ( $($req,)* ) ( $($opt,)* ));
    };

    // start tail with map
    (@fn_req $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* )
        #[tail] #[map($conv:path)] $(#[$_other:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::tail_define!(@fn_opt $mac => $func ( $($req,)* ) ( $($opt,)* ( $name : $ty => $conv ), ) $($rest)* );
    };
    (@fn_req $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* )
        #[tail] $(#[$_other:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::tail_define!(@fn_opt $mac => $func ( $($req,)* ) ( $($opt,)* ( $name : $ty ), ) $($rest)* );
    };

    // required param
    (@fn_req $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::tail_define!(@fn_req $mac => $func ( $($req,)* $name, ) ( $($opt,)* ) $($rest)* );
    };
    (@fn_req $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::tail_define!(@fn_finish $mac => $func ( $($req,)* $name, ) ( $($opt,)* ));
    };

    // optional mode
    (@fn_opt $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* ) , $($rest:tt)* ) => {
        $crate::tail_define!(@fn_opt $mac => $func ( $($req,)* ) ( $($opt,)* ) $($rest)* );
    };
    (@fn_opt $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* )) => {
        $crate::tail_define!(@fn_finish $mac => $func ( $($req,)* ) ( $($opt,)* ));
    };
    (@fn_opt $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* )
        #[map($conv:path)] $(#[$_other:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::tail_define!(@fn_opt $mac => $func ( $($req,)* ) ( $($opt,)* ( $name : $ty => $conv ), ) $($rest)* );
    };
    (@fn_opt $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty , $($rest:tt)*
    ) => {
        $crate::tail_define!(@fn_opt $mac => $func ( $($req,)* ) ( $($opt,)* ( $name : $ty ), ) $($rest)* );
    };
    (@fn_opt $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* )
        #[map($conv:path)] $(#[$_other:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::tail_define!(@fn_finish $mac => $func ( $($req,)* ) ( $($opt,)* ( $name : $ty => $conv ), ));
    };
    (@fn_opt $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* )
        $(#[$_attr:meta])*
        $name:ident : $ty:ty
    ) => {
        $crate::tail_define!(@fn_finish $mac => $func ( $($req,)* ) ( $($opt,)* ( $name : $ty ), ));
    };

    (@fn_finish $mac:ident => $func:ident ( $($req:ident ,)* ) ( $($opt:tt ,)* )) => {
        $crate::tail_define!(@define_generated_fn_macro $mac => $func ( $($req),* ) ( $($opt),* ));
    };

    (@define_generated_fn_macro $mac:ident => $func:ident ( $($req_name:ident),* ) ( $($opt_specs:tt),* )) => {
        macro_rules! __define_tail_optional_fn_proto_macro_with_dollar {
            ($d:tt) => {
                #[macro_export]
                macro_rules! $mac {
                    ($($d $req_name:expr),*) => {{
                        $crate::__tail_omittable_munch!(@munch
                            fn,
                            ($func),
                            ( $($d $req_name),* ),
                            (),
                            ( $($opt_specs),* ),
                            ()
                        )
                    }};
                    ($($d $req_name:expr),*,) => {{
                        $crate::__tail_omittable_munch!(@munch
                            fn,
                            ($func),
                            ( $($d $req_name),* ),
                            (),
                            ( $($opt_specs),* ),
                            ()
                        )
                    }};
                    ($($d $req_name:expr),*, $d( $d provided:tt )+ ) => {{
                        $crate::__tail_omittable_munch!(@munch
                            fn,
                            ($func),
                            ( $($d $req_name),* ),
                            ( $d( $d provided )+ ),
                            ( $($opt_specs),* ),
                            ()
                        )
                    }};
                    ($($d $req_name:expr),*, $d( $d provided:tt )+ ,) => {{
                        $crate::__tail_omittable_munch!(@munch
                            fn,
                            ($func),
                            ( $($d $req_name),* ),
                            ( $d( $d provided )+ , ),
                            ( $($opt_specs),* ),
                            ()
                        )
                    }};
                }
            };
        }
        __define_tail_optional_fn_proto_macro_with_dollar!($);
    };

    // Public entrypoint (kept last so it doesn't steal internal @-rules).
    ( $($tt:tt)* ) => {
        $crate::tail_define!(@items $($tt)*);
    };
}


