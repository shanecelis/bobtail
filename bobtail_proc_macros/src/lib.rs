#![doc(html_root_url = "https://docs.rs/bobtail/0.1.0")]
#![doc = include_str!("../README.md")]
#![forbid(missing_docs)]
#![forbid(unsafe_code)]

use proc_macro::TokenStream;
use proc_macro2::{Ident as Ident2, Span};
#[cfg(not(test))]
use proc_macro_crate::{crate_name, FoundCrate};
use proc_macro_warning::Warning;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    // parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    Attribute, Error, Ident, ImplItem, ImplItemMethod, Item, ItemFn, Meta, NestedMeta, Pat, Result,
    Token, Visibility,
};

fn bobtail_path() -> Result<proc_macro2::TokenStream> {
    #[cfg(test)]
    {
        // In tests, return a mock path to avoid dependency lookup errors
        return Ok(quote!(::bobtail));
    }
    
    #[cfg(not(test))]
    {
        let found = crate_name("bobtail")
            .map_err(|e| Error::new(Span::call_site(), format!("proc-macro-crate error: {e}")))?;
        Ok(match found {
            // We want a path that works even when this proc-macro is invoked from a
            // binary target in the same package (examples), where `crate` would
            // refer to the binary crate, not the library crate.
            FoundCrate::Itself => quote!(::bobtail),
            FoundCrate::Name(name) => {
                let ident = Ident::new(&name, Span::call_site());
                quote!(::#ident)
            }
        })
    }
}

#[derive(Debug)]
struct MethodSpec {
    method: Ident,
    macro_name: Option<Ident>,
    // conv: Vec<(Ident, Path)>,
}

impl MethodSpec {
    fn new(method: Ident) -> Self {
        Self {
            method,
            macro_name: None, //conv: Vec::new()
        }
    }
}

fn strip_block_attr(attrs: &mut Vec<Attribute>) {
    attrs.retain(|a| !a.path.is_ident("block"));
}

fn path_last_ident(path: &syn::Path) -> Option<&syn::Ident> {
    path.segments.last().map(|s| &s.ident)
}

fn is_tail_attr(a: &Attribute) -> bool {
    path_last_ident(&a.path).is_some_and(|id| id == "tail")
}

fn is_map_attr(a: &Attribute) -> bool {
    path_last_ident(&a.path).is_some_and(|id| id == "map")
}

fn is_bob_attr(a: &Attribute) -> bool {
    path_last_ident(&a.path).is_some_and(|id| id == "bob")
}

fn receiver_tokens(sig: &syn::Signature) -> Option<proc_macro2::TokenStream> {
    let syn::FnArg::Receiver(r) = sig.inputs.iter().next()? else {
        return None;
    };
    Some(match (&r.reference, &r.mutability) {
        (Some(_), Some(_)) => quote!(&mut self),
        (Some(_), None) => quote!(&self),
        (None, Some(_)) => quote!(mut self),
        (None, None) => quote!(self),
    })
}

fn parse_attr_args(attr: &Attribute) -> Result<Vec<NestedMeta>> {
    // Support `#[bobtail::bob]` with no parentheses.
    if attr.tokens.is_empty() {
        return Ok(Vec::new());
    }
    let punct: Punctuated<NestedMeta, Token![,]> =
        attr.parse_args_with(Punctuated::<NestedMeta, Token![,]>::parse_terminated)?;
    Ok(punct.into_iter().collect())
}

fn parse_bob_attr_args(attr: proc_macro2::TokenStream) -> Result<Vec<NestedMeta>> {
    if attr.is_empty() {
        return Ok(Vec::new());
    }
    struct BobArgs(Punctuated<NestedMeta, Token![,]>);
    impl Parse for BobArgs {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(Self(Punctuated::<NestedMeta, Token![,]>::parse_terminated(
                input,
            )?))
        }
    }
    let BobArgs(punct) = syn::parse2::<BobArgs>(attr)?;
    Ok(punct.into_iter().collect())
}

/// Tag functions or methods that have a `#[tail]`, which can be omitted.
/// attributes on methods.
#[proc_macro_attribute]
pub fn bob(attr: TokenStream, item: TokenStream) -> TokenStream {
    bob_impl(attr.into(), item.into()).into()
}

/// Internal implementation that works with proc_macro2 for testing
fn bob_impl(attr: proc_macro2::TokenStream, item: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    if let Ok(mut fun) = syn::parse2::<ItemFn>(item.clone()) {
        // If it's a free function, generate the macro proxy right here.
        let crate_path = match bobtail_path() {
            Ok(p) => p,
            Err(e) => return e.to_compile_error().into(),
        };

        let bob_args: Vec<NestedMeta> = match parse_bob_attr_args(attr) {
            Ok(v) => v,
            Err(e) => return e.to_compile_error().into(),
        };

        let mut macro_name: Option<Ident> = None;
        if let Some(NestedMeta::Meta(Meta::Path(p))) = bob_args.get(0) {
            if let Some(id) = p.get_ident() {
                macro_name = Some(id.clone());
            }
        }
        let macro_name = macro_name.unwrap_or_else(|| fun.sig.ident.clone());

        // Receiver isn't allowed in free functions, but syn would parse `self` anyway
        // as a typed arg (and later fail typechecking). Reject it explicitly.
        if fun
            .sig
            .inputs
            .iter()
            .any(|a| matches!(a, syn::FnArg::Receiver(_)))
        {
            return Error::new(
                fun.sig.span(),
                "#[bobtail::bob] on free functions cannot use a self receiver",
            )
            .to_compile_error()
            .into();
        }

        // Collect typed params and find tail start.
        let typed: Vec<&syn::PatType> = fun
            .sig
            .inputs
            .iter()
            .filter_map(|a| match a {
                syn::FnArg::Typed(p) => Some(p),
                syn::FnArg::Receiver(_) => None,
            })
            .collect();

        let mut tail_start: Option<usize> = None;
        for (i, p) in typed.iter().enumerate() {
            if p.attrs.iter().any(is_tail_attr) {
                tail_start = Some(i);
                break;
            }
        }

        // Emit a warning if no #[tail] attribute is present
        let warning = if tail_start.is_none() && !typed.is_empty() {
            Some(
                Warning::new_deprecated(&format!("{}_no_tail", fun.sig.ident))
                    .old(&format!(
                        "using `{}` without `#[tail]` attribute",
                        fun.sig.ident
                    ))
                    .new(&format!(
                        "add `#[tail]` to make trailing arguments optional"
                    ))
                    .span(fun.sig.ident.span())
                    .build_or_panic(),
            )
        } else {
            None
        };

        // No default tail - if no #[tail] is present, all args are required
        let req_count = tail_start.unwrap_or(typed.len());
        let tail_count = tail_start
            .map(|i| typed.len().saturating_sub(i))
            .unwrap_or(0);

        // Strip marker attrs so the compiler never needs to resolve them.
        fun.attrs.retain(|a| !is_bob_attr(a));
        for arg in &mut fun.sig.inputs {
            let syn::FnArg::Typed(pat_ty) = arg else {
                continue;
            };
            pat_ty.attrs.retain(|a| !is_tail_attr(a) && !is_map_attr(a));
        }

        // Emit macro that delegates to bobtail's internal muncher.
        let req_idents: Vec<Ident2> = (0..req_count)
            .map(|i| Ident2::new(&format!("__bobtail_req_{i}"), Span::call_site()))
            .collect();
        let tail_idents: Vec<Ident2> = (0..tail_count)
            .map(|i| Ident2::new(&format!("__bobtail_tail_{i}"), Span::call_site()))
            .collect();
        let fn_name = &fun.sig.ident;

        let out = quote! {
            #fun
            #warning
            macro_rules! #macro_name {
                ($($call:tt)*) => {
                    #crate_path::__bobtail_munch!(
                        #fn_name,
                        no_receiver,
                        ( #(#req_idents,)* ),
                        ( #(#tail_idents,)* );
                        $($call)*
                    )
                };
            }
        };
        // println!("BOB {}", &out);
        out.into()
    } else {
        // Otherwise, treat it as an `impl` method marker (consumed by `#[bobtail::block]`).
        let mut method: ImplItemMethod = match syn::parse2(item.clone()) {
            Ok(m) => m,
            Err(e) => return e.to_compile_error(),
        };
        method.attrs.retain(|a| !is_bob_attr(a));
        quote!(#method)
    }
}

/// Tag `impl`-blocks to enable specifying `#[bob]` and `#[tail]` attributes on methods.
#[proc_macro_attribute]
pub fn block(_attr: TokenStream, item: TokenStream) -> TokenStream {
    block_impl(_attr.into(), item.into()).into()
}

/// Internal implementation that works with proc_macro2 for testing
fn block_impl(_attr: proc_macro2::TokenStream, item: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let item_ts: proc_macro2::TokenStream = item.clone();

    let crate_path = match bobtail_path() {
        Ok(p) => p,
        Err(e) => return e.to_compile_error().into(),
    };

    let parsed: Item = match syn::parse2(item_ts.clone()) {
        Ok(it) => it,
        Err(e) => return e.to_compile_error().into(),
    };

    match parsed {
        Item::Impl(mut item_impl) => {
            let mut items = Vec::<proc_macro2::TokenStream>::new();
            let mut warnings = Vec::<proc_macro2::TokenStream>::new();
            let mut warning_counter = 0u64;

            for it in &item_impl.items {
                let ImplItem::Method(method_fn) = it else {
                    continue;
                };

                // Find `#[bobtail::bob]` marker on this method.
                let mut found_bob: Option<Attribute> = None;
                for a in &method_fn.attrs {
                    if is_bob_attr(a) {
                        found_bob = Some(a.clone());
                        break;
                    }
                }
                let Some(bob_attr) = found_bob else { continue };

                let bob_args: Vec<NestedMeta> = match parse_attr_args(&bob_attr) {
                    Ok(v) => v,
                    Err(e) => return e.to_compile_error().into(),
                };

                // Default macro name = method name, unless `#[bobtail::bob(name)]`.
                let mut spec = MethodSpec::new(method_fn.sig.ident.clone());
                if let Some(NestedMeta::Meta(Meta::Path(p))) = bob_args.get(0) {
                    if let Some(id) = p.get_ident() {
                        spec.macro_name = Some(id.clone());
                    }
                }

                let Some(receiver) = receiver_tokens(&method_fn.sig) else {
                    return Error::new(
                        method_fn.sig.span(),
                        "bobtail::block currently requires a self receiver",
                    )
                    .to_compile_error()
                    .into();
                };

                // Build prototype params by largely reusing the method signature + markers.
                let mut proto_params: Vec<proc_macro2::TokenStream> = Vec::new();
                proto_params.push(quote!(#receiver,));

                // Track the first #[bobtail::tail] to emit #[tail] exactly once.
                let mut tail_started = false;
                let mut has_any_tail = false;

                for arg in method_fn.sig.inputs.iter().skip(1) {
                    let syn::FnArg::Typed(pat_ty) = arg else {
                        continue;
                    };
                    let Pat::Ident(pat_ident) = pat_ty.pat.as_ref() else {
                        return Error::new(
                            pat_ty.pat.span(),
                            "bobtail only supports ident parameters",
                        )
                        .to_compile_error()
                        .into();
                    };
                    let name = &pat_ident.ident;
                    let ty = pat_ty.ty.as_ref();

                    let has_tail = pat_ty.attrs.iter().any(is_tail_attr);
                    if has_tail {
                        has_any_tail = true;
                    }
                    // let mut map_paths: Vec<Path> = Vec::new();
                    // for a in &pat_ty.attrs {
                    //     if is_map_attr(a) {
                    //         let p: Path = match a.parse_args() {
                    //             Ok(p) => p,
                    //             Err(e) => return e.to_compile_error().into(),
                    //         };
                    //         map_paths.push(p);
                    //     }
                    // }

                    let mut markers: Vec<proc_macro2::TokenStream> = Vec::new();
                    if has_tail && !tail_started {
                        markers.push(quote!(#[tail]));
                        tail_started = true;
                    }
                    // for p in map_paths {
                    //     markers.push(quote!(#[map(#p)]));
                    //     spec.conv.push((name.clone(), p));
                    // }

                    proto_params.push(quote!(#(#markers)* #name : #ty,));
                }

                // Emit a warning if no #[tail] attribute is present
                // Check if there are any non-receiver arguments
                let method_warning = if !has_any_tail
                    && method_fn.sig.inputs.iter().skip(1).next().is_some()
                {
                    // Use a unique identifier with a counter to avoid conflicts
                    let unique_id = format!("{}_{}_no_tail", method_fn.sig.ident, warning_counter);
                    warning_counter += 1;
                    Some(
                        Warning::new_deprecated(&unique_id)
                            .old(&format!(
                                "using `{}` without `#[tail]` attribute",
                                method_fn.sig.ident
                            ))
                            .new(&format!(
                                "add `#[tail]` to make trailing arguments optional"
                            ))
                            .span(method_fn.sig.ident.span())
                            .build_or_panic(),
                    )
                } else {
                    None
                };

                let out_ty = match &method_fn.sig.output {
                    syn::ReturnType::Default => None,
                    syn::ReturnType::Type(_, ty) => Some(ty.as_ref()),
                };

                // Check if the method is public
                let is_public = matches!(method_fn.vis, Visibility::Public(_));

                // Emit as a define item (method prototype).
                let method = &spec.method;
                // Store warnings separately to emit before the define! block
                if let Some(warning) = method_warning {
                    warnings.push(quote!(#warning));
                }
                
                // Add #[macro_export] attribute if the method is public
                // Format it properly so it's parsed as an outer attribute
                if let Some(mac) = &spec.macro_name {
                    if let Some(ret) = out_ty {
                        if is_public {
                            items.push(quote!(#[macro_export] #mac => fn #method( #(#proto_params)* ) -> #ret;));
                        } else {
                            items.push(quote!(#mac => fn #method( #(#proto_params)* ) -> #ret;));
                        }
                    } else {
                        if is_public {
                            items.push(quote!(#[macro_export] #mac => fn #method( #(#proto_params)* );));
                        } else {
                            items.push(quote!(#mac => fn #method( #(#proto_params)* );));
                        }
                    }
                } else if let Some(ret) = out_ty {
                    if is_public {
                        items.push(quote!(#[macro_export] fn #method( #(#proto_params)* ) -> #ret;));
                    } else {
                        items.push(quote!(fn #method( #(#proto_params)* ) -> #ret;));
                    }
                } else {
                    if is_public {
                        items.push(quote!(#[macro_export] fn #method( #(#proto_params)* );));
                    } else {
                        items.push(quote!(fn #method( #(#proto_params)* );));
                    }
                }
            }

            // Strip marker attrs from the output impl so the compiler never has to resolve them.
            for it in &mut item_impl.items {
                let ImplItem::Method(method_fn) = it else {
                    continue;
                };
                method_fn.attrs.retain(|a| !is_bob_attr(a));
                for arg in &mut method_fn.sig.inputs {
                    let syn::FnArg::Typed(pat_ty) = arg else {
                        continue;
                    };
                    pat_ty.attrs.retain(|a| !is_tail_attr(a) && !is_map_attr(a));
                }
            }

            strip_block_attr(&mut item_impl.attrs);

            let tail_define_block = if items.is_empty() {
                quote!()
            } else {
                quote!(#crate_path::define!( #(#items)* );)
            };

            let out = quote! {
                #item_impl
                #(#warnings)*
                #tail_define_block
            };
            // eprintln!("BLOCK {}", &out);
            out.into()
        }
        other => Error::new(
            other.span(),
            "bobtail::block can only be used on an impl block",
        )
        .to_compile_error()
        .into(),
    }
}

// ---- define proc macro (prototype -> macro_rules wrapper generator) ----

fn is_tail_marker(attrs: &[Attribute]) -> bool {
    attrs.iter().any(is_tail_attr)
}

#[derive(Debug)]
struct ProtoItem {
    outer_attrs: Vec<Attribute>,
    macro_name: Ident,
    fn_name: Ident,
    has_receiver: bool,
    req_count: usize,
    tail_count: usize,
}

impl syn::parse::Parse for ProtoItem {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let outer_attrs = input.call(Attribute::parse_outer)?;

        // Optional: `mac_name =>`
        let mut macro_name: Option<Ident> = None;
        if input.peek(Ident) {
            let fork = input.fork();
            let _maybe: Ident = fork.parse()?;
            if fork.peek(Token![=>]) {
                let mac: Ident = input.parse()?;
                input.parse::<Token![=>]>()?;
                macro_name = Some(mac);
            }
        }

        input.parse::<Token![fn]>()?;
        let fn_name: Ident = input.parse()?;

        let content;
        syn::parenthesized!(content in input);
        let args: Punctuated<syn::FnArg, Token![,]> =
            content.parse_terminated(syn::FnArg::parse)?;

        let mut has_receiver = false;
        let mut typed: Vec<syn::PatType> = Vec::new();
        for (i, a) in args.into_iter().enumerate() {
            match a {
                syn::FnArg::Receiver(_) => {
                    if i != 0 {
                        return Err(Error::new(
                            fn_name.span(),
                            "receiver must be the first parameter",
                        ));
                    }
                    has_receiver = true;
                }
                syn::FnArg::Typed(p) => typed.push(p),
            }
        }

        // Find the first #[tail] marker (if any).
        let mut tail_start: Option<usize> = None;
        for (i, p) in typed.iter().enumerate() {
            if is_tail_marker(&p.attrs) {
                tail_start = Some(i);
                break;
            }
        }

        let req_count = match tail_start {
            Some(i) => i,
            None => typed.len(),
        };
        let tail_count = match tail_start {
            Some(i) => typed.len().saturating_sub(i),
            None => 0,
        };

        // Parse optional return type (ignored) then require semicolon.
        if input.peek(Token![->]) {
            input.parse::<Token![->]>()?;
            let _ty: syn::Type = input.parse()?;
        }
        input.parse::<Token![;]>()?;

        Ok(Self {
            outer_attrs,
            macro_name: macro_name.unwrap_or_else(|| fn_name.clone()),
            fn_name,
            has_receiver,
            req_count,
            tail_count,
        })
    }
}

#[derive(Debug)]
struct TailDefineInput {
    items: Vec<ProtoItem>,
}

impl syn::parse::Parse for TailDefineInput {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let mut items = Vec::new();
        while !input.is_empty() {
            items.push(input.parse::<ProtoItem>()?);
        }
        Ok(Self { items })
    }
}

/// Specify function or method prototypes that generates a variadic macro proxy
/// allow for omission of elements in its "tail".
#[proc_macro]
pub fn define(input: TokenStream) -> TokenStream {
    define_impl(input.into()).into()
}

/// Internal implementation that works with proc_macro2 for testing
fn define_impl(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let crate_path = match bobtail_path() {
        Ok(p) => p,
        Err(e) => return e.to_compile_error().into(),
    };

    let parsed: TailDefineInput = match syn::parse2(input.clone()) {
        Ok(p) => p,
        Err(e) => return e.to_compile_error(),
    };
    let mut out = proc_macro2::TokenStream::new();

    for item in parsed.items {
        let ProtoItem {
            outer_attrs,
            macro_name,
            fn_name,
            has_receiver,
            req_count,
            tail_count,
        } = item;

        // Note: We don't emit warnings here in define! because:
        // 1. When called from block!, warnings are already emitted there
        // 2. When called directly by users, they should use #[bob] or #[block] which emit warnings
        // This avoids duplicate warnings

        let recv_tok = if has_receiver {
            quote!(receiver)
        } else {
            quote!(no_receiver)
        };

        let req_idents: Vec<Ident> = (0..req_count)
            .map(|i| Ident::new(&format!("__bobtail_req_{i}"), Span::call_site()))
            .collect();
        let tail_idents: Vec<Ident> = (0..tail_count)
            .map(|i| Ident::new(&format!("__bobtail_tail_{i}"), Span::call_site()))
            .collect();

        // If #[macro_export] is in outer_attrs, use it; otherwise don't add it
        // The attribute should have been parsed correctly by Attribute::parse_outer
        out.extend(quote! {
            #(#outer_attrs)*
            macro_rules! #macro_name {
                ($($call:tt)*) => {
                    #crate_path::__bobtail_munch!(
                        #fn_name,
                        #recv_tok,
                        ( #(#req_idents,)* ),
                        ( #(#tail_idents,)* );
                        $($call)*
                    )
                };
            }
        });
    }

    // eprintln!("DEFINE {}", &out);
    // dbg!(out.into())
    out.into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use proc_macro2::TokenStream;

    #[test]
    fn test_bob_macro_output() {
        let input = r#"
            fn f(a: u8, #[tail] b: Option<u8>) -> u8 {
                b.map(|x| x + a).unwrap_or(a)
            }
        "#;
        
        let input_ts: TokenStream = input.parse().unwrap();
        let empty_attr: TokenStream = TokenStream::new();
        
        // Call the internal implementation that works with proc_macro2
        let output = bob_impl(empty_attr, input_ts);
        let output_str = output.to_string();
        
        // Expected output: function definition + macro_rules! definition
        let expected = r#"fn f (a : u8 , b : Option < u8 >) -> u8 { b . map (| x | x + a) . unwrap_or (a) } macro_rules ! f { ($ ($ call : tt) *) => { :: bobtail :: __bobtail_munch ! (f , no_receiver , (__bobtail_req_0 ,) , (__bobtail_tail_0 ,) ; $ ($ call) *) } ; }"#;
        assert_eq!(output_str.trim(), expected.trim());
    }

    #[test]
    fn test_block_macro_output() {
        let input = r#"
            impl A {
                #[bobtail::bob]
                pub fn b(&self, a: u8, #[tail] b: Option<u8>) -> u8 {
                    b.map(|x| x + a).unwrap_or(a)
                }
            }
        "#;
        
        let input_ts: TokenStream = input.parse().unwrap();
        let empty_attr: TokenStream = TokenStream::new();
        
        let output = block_impl(empty_attr, input_ts);
        let output_str = output.to_string();
        
        // Expected output: impl block + define! macro call
        let expected = r#"impl A { pub fn b (& self , a : u8 , b : Option < u8 >) -> u8 { b . map (| x | x + a) . unwrap_or (a) } } :: bobtail :: define ! (# [macro_export] fn b (& self , a : u8 , # [tail] b : Option < u8 > ,) -> u8 ;) ;"#;
        assert_eq!(output_str.trim(), expected.trim());
    }

    #[test]
    fn test_define_macro_output() {
        let input = r#"
            fn f(a: u8, #[tail] b: Option<u8>) -> u8;
        "#;
        
        let input_ts: TokenStream = input.parse().unwrap();
        let output = define_impl(input_ts);
        let output_str = output.to_string();
        
        // Expected output: macro_rules! definition
        let expected = r#"macro_rules ! f { ($ ($ call : tt) *) => { :: bobtail :: __bobtail_munch ! (f , no_receiver , (__bobtail_req_0 ,) , (__bobtail_tail_0 ,) ; $ ($ call) *) } ; }"#;
        assert_eq!(output_str.trim(), expected.trim());
    }
}
