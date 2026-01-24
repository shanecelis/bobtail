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

        // Generate explicit match arms for each possible argument count
        let fn_name = &fun.sig.ident;
        let mut match_arms = proc_macro2::TokenStream::new();
        
        for provided_tail_count in 0..=tail_count {
            // Pattern: required args + provided tail args
            let mut pattern_parts = Vec::new();
            let mut call_args = Vec::new();
            
            // Add required argument patterns
            for i in 0..req_count {
                let ident = Ident::new(&format!("arg_{}", i), Span::call_site());
                pattern_parts.push(quote!($#ident:expr));
                call_args.push(quote!($#ident));
            }
            
            // Add provided tail argument patterns
            for i in 0..provided_tail_count {
                let ident = Ident::new(&format!("tail_{}", i), Span::call_site());
                pattern_parts.push(quote!($#ident:expr));
                call_args.push(quote!(::core::convert::From::from($#ident)));
            }
            
            // Add defaulted tail arguments
            for _ in provided_tail_count..tail_count {
                call_args.push(quote!(::core::default::Default::default()));
            }
            
            // Generate the match arm pattern (without trailing comma)
            use proc_macro2::{Delimiter, TokenTree};
            let pattern = if pattern_parts.is_empty() {
                quote!(())
            } else {
                let mut inner = proc_macro2::TokenStream::new();
                
                let mut first = true;
                for part in pattern_parts.iter() {
                    if !first {
                        inner.extend(quote!(,));
                    }
                    inner.extend(part.clone());
                    first = false;
                }
                
                let group = proc_macro2::Group::new(Delimiter::Parenthesis, inner);
                let mut pat_ts = proc_macro2::TokenStream::new();
                pat_ts.extend(std::iter::once(TokenTree::Group(group)));
                pat_ts
            };
            
            // Generate the function call (all args in one call, no trailing comma)
            let call = if call_args.is_empty() {
                quote!(#fn_name())
            } else {
                let first_arg = call_args.first().unwrap();
                let mut args_ts = quote!(#first_arg);
                for arg in call_args.iter().skip(1) {
                    args_ts.extend(quote!(, #arg));
                }
                quote!(#fn_name(#args_ts))
            };
            
            match_arms.extend(quote! {
                #pattern => {
                    #call
                };
            });
        }
        
        // Add catch-all pattern to handle `_` token in tail positions
        if tail_count > 0 {
            let mut catch_all_pattern = Vec::new();
            let mut catch_all_call = Vec::new();
            
            for i in 0..req_count {
                let ident = Ident::new(&format!("arg_{}", i), Span::call_site());
                catch_all_pattern.push(quote!($#ident:expr));
                catch_all_call.push(quote!($#ident));
            }
            
            for i in 0..tail_count {
                let ident = Ident::new(&format!("tail_{}", i), Span::call_site());
                catch_all_pattern.push(quote!($#ident:tt));
                catch_all_call.push(quote!(#crate_path::__bobtail_handle_underscore!($#ident)));
            }
            
            use proc_macro2::{Delimiter, TokenTree};
            let pattern = if catch_all_pattern.is_empty() {
                quote!(())
            } else {
                let mut inner = proc_macro2::TokenStream::new();
                let mut first = true;
                for part in catch_all_pattern.iter() {
                    if !first {
                        inner.extend(quote!(,));
                    }
                    inner.extend(part.clone());
                    first = false;
                }
                let group = proc_macro2::Group::new(Delimiter::Parenthesis, inner);
                let mut pat_ts = proc_macro2::TokenStream::new();
                pat_ts.extend(std::iter::once(TokenTree::Group(group)));
                pat_ts
            };
            
            let call = if catch_all_call.is_empty() {
                quote!(#fn_name())
            } else {
                let first_arg = catch_all_call.first().unwrap();
                let mut args_ts = quote!(#first_arg);
                for arg in catch_all_call.iter().skip(1) {
                    args_ts.extend(quote!(, #arg));
                }
                quote!(#fn_name(#args_ts))
            };
            
            match_arms.extend(quote! {
                #pattern => {
                    #call
                };
            });
        }

        let out = quote! {
            #fun
            #warning
            macro_rules! #macro_name {
                #match_arms
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

                // Collect typed params and find tail start
                let typed: Vec<&syn::PatType> = method_fn
                    .sig
                    .inputs
                    .iter()
                    .skip(1)
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

                let req_count = tail_start.unwrap_or(typed.len());
                let tail_count = tail_start
                    .map(|i| typed.len().saturating_sub(i))
                    .unwrap_or(0);

                // Emit a warning if no #[tail] attribute is present
                let method_warning = if tail_start.is_none() && !typed.is_empty() {
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

                // Store warnings separately
                if let Some(warning) = method_warning {
                    warnings.push(quote!(#warning));
                }

                // Check if the method is public
                let is_public = matches!(method_fn.vis, Visibility::Public(_));

                // Generate explicit match arms for the macro
                let macro_name = &spec.macro_name.unwrap_or_else(|| method_fn.sig.ident.clone());
                let fn_name = &method_fn.sig.ident;
                
                // Generate explicit match arms for each possible argument count
                let mut match_arms = proc_macro2::TokenStream::new();
                
                for provided_tail_count in 0..=tail_count {
                    // Pattern: receiver + required args + provided tail args
                    let mut pattern_parts = Vec::new();
                    let mut call_args = Vec::new();
                    
                    // Add receiver pattern (always present for methods)
                    pattern_parts.push(quote!($self_:expr));
                    
                    // Add required argument patterns
                    for i in 0..req_count {
                        let ident = Ident::new(&format!("arg_{}", i), Span::call_site());
                        pattern_parts.push(quote!($#ident:expr));
                        call_args.push(quote!($#ident));
                    }
                    
                    // Add provided tail argument patterns
                    for i in 0..provided_tail_count {
                        let ident = Ident::new(&format!("tail_{}", i), Span::call_site());
                        pattern_parts.push(quote!($#ident:expr));
                        call_args.push(quote!(::core::convert::From::from($#ident)));
                    }
                    
                    // Add defaulted tail arguments
                    for _ in provided_tail_count..tail_count {
                        call_args.push(quote!(::core::default::Default::default()));
                    }
                    
                    // Generate the match arm pattern (without trailing comma)
                    use proc_macro2::{Delimiter, TokenTree};
                    let mut inner = proc_macro2::TokenStream::new();
                    
                    let mut first = true;
                    for part in pattern_parts.iter() {
                        if !first {
                            inner.extend(quote!(,));
                        }
                        inner.extend(part.clone());
                        first = false;
                    }
                    
                    let group = proc_macro2::Group::new(Delimiter::Parenthesis, inner);
                    let mut pat_ts = proc_macro2::TokenStream::new();
                    pat_ts.extend(std::iter::once(TokenTree::Group(group)));
                    let pattern = pat_ts;
                    
                    // Generate the function call (all args in one call, no trailing comma)
                    let call = if call_args.is_empty() {
                        quote!($self_.#fn_name())
                    } else {
                        let first_arg = call_args.first().unwrap();
                        let mut args_ts = quote!(#first_arg);
                        for arg in call_args.iter().skip(1) {
                            args_ts.extend(quote!(, #arg));
                        }
                        quote!($self_.#fn_name(#args_ts))
                    };
                    
                    match_arms.extend(quote! {
                        #pattern => {
                            #call
                        };
                    });
                }
                
                // Add catch-all pattern to handle `_` token in tail positions
                if tail_count > 0 {
                    let mut catch_all_pattern = Vec::new();
                    let mut catch_all_call = Vec::new();
                    
                    catch_all_pattern.push(quote!($self_:expr));
                    
                    for i in 0..req_count {
                        let ident = Ident::new(&format!("arg_{}", i), Span::call_site());
                        catch_all_pattern.push(quote!($#ident:expr));
                        catch_all_call.push(quote!($#ident));
                    }
                    
                    for i in 0..tail_count {
                        let ident = Ident::new(&format!("tail_{}", i), Span::call_site());
                        catch_all_pattern.push(quote!($#ident:tt));
                        catch_all_call.push(quote!(#crate_path::__bobtail_handle_underscore!($#ident)));
                    }
                    
                    use proc_macro2::{Delimiter, TokenTree};
                    let pattern = {
                        let mut inner = proc_macro2::TokenStream::new();
                        let mut first = true;
                        for part in catch_all_pattern.iter() {
                            if !first {
                                inner.extend(quote!(,));
                            }
                            inner.extend(part.clone());
                            first = false;
                        }
                        let group = proc_macro2::Group::new(Delimiter::Parenthesis, inner);
                        let mut pat_ts = proc_macro2::TokenStream::new();
                        pat_ts.extend(std::iter::once(TokenTree::Group(group)));
                        pat_ts
                    };
                    
                    let call = {
                        let first_arg = catch_all_call.first().unwrap();
                        let mut args_ts = quote!(#first_arg);
                        for arg in catch_all_call.iter().skip(1) {
                            args_ts.extend(quote!(, #arg));
                        }
                        quote!($self_.#fn_name(#args_ts))
                    };
                    
                    match_arms.extend(quote! {
                        #pattern => {
                            #call
                        };
                    });
                }
                
                // Generate the macro_rules! with explicit match arms
                let macro_def = if is_public {
                    quote! {
                        #[macro_export]
                        macro_rules! #macro_name {
                            #match_arms
                        }
                    }
                } else {
                    quote! {
                        macro_rules! #macro_name {
                            #match_arms
                        }
                    }
                };
                
                items.push(macro_def);
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

            let out = quote! {
                #item_impl
                #(#warnings)*
                #(#items)*
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

        // Generate explicit match arms for each possible argument count
        // Also generate arms that handle `_` as a special token meaning "use default"
        let mut match_arms = proc_macro2::TokenStream::new();
        
        // Generate arms for each number of provided tail args (0 to tail_count)
        for provided_tail_count in 0..=tail_count {
            // Pattern: receiver (if any) + required args + provided tail args
            let mut pattern_parts = Vec::new();
            let mut call_args = Vec::new();
            
            // Add receiver pattern if present
            if has_receiver {
                pattern_parts.push(quote!($self_:expr));
            }
            
            // Add required argument patterns
            for i in 0..req_count {
                let ident = Ident::new(&format!("arg_{}", i), Span::call_site());
                pattern_parts.push(quote!($#ident:expr));
                call_args.push(quote!($#ident));
            }
            
            // Add provided tail argument patterns
            for i in 0..provided_tail_count {
                let ident = Ident::new(&format!("tail_{}", i), Span::call_site());
                pattern_parts.push(quote!($#ident:expr));
                call_args.push(quote!(::core::convert::From::from($#ident)));
            }
            
            // Add defaulted tail arguments
            for _ in provided_tail_count..tail_count {
                call_args.push(quote!(::core::default::Default::default()));
            }
            
            // Generate the match arm pattern (without trailing comma)
            use proc_macro2::{Delimiter, TokenTree};
            let pattern = if pattern_parts.is_empty() {
                quote!(())
            } else {
                let mut inner = proc_macro2::TokenStream::new();
                
                let mut first = true;
                for part in pattern_parts.iter() {
                    if !first {
                        inner.extend(quote!(,));
                    }
                    inner.extend(part.clone());
                    first = false;
                }
                
                let group = proc_macro2::Group::new(Delimiter::Parenthesis, inner);
                let mut pat_ts = proc_macro2::TokenStream::new();
                pat_ts.extend(std::iter::once(TokenTree::Group(group)));
                pat_ts
            };
            
            // Generate the function call (all args in one call, no trailing comma)
            let call = if call_args.is_empty() {
                if has_receiver {
                    quote!($self_.#fn_name())
                } else {
                    quote!(#fn_name())
                }
            } else {
                let first_arg = call_args.first().unwrap();
                let mut args_ts = quote!(#first_arg);
                for arg in call_args.iter().skip(1) {
                    args_ts.extend(quote!(, #arg));
                }
                
                if has_receiver {
                    quote!($self_.#fn_name(#args_ts))
                } else {
                    quote!(#fn_name(#args_ts))
                }
            };
            
            match_arms.extend(quote! {
                #pattern => {
                    #call
                };
            });
        }
        
        // Note: `_` cannot be matched in macro_rules! patterns (it's reserved)
        // The explicit patterns above handle all cases where expressions are provided
        // For `_` support, we would need recursive macro expansion like the old `__bobtail_munch!`
        // For now, users should omit arguments instead of using `_`
        
        // If #[macro_export] is in outer_attrs, use it; otherwise don't add it
        // The attribute should have been parsed correctly by Attribute::parse_outer
        out.extend(quote! {
            #(#outer_attrs)*
            macro_rules! #macro_name {
                #match_arms
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
        
        // Expected output: function definition + explicit macro_rules! with match arms
        let expected = r#"fn f (a : u8 , b : Option < u8 >) -> u8 { b . map (| x | x + a) . unwrap_or (a) } macro_rules ! f { ($ arg_0 : expr) => { f ($ arg_0 , :: core :: default :: Default :: default ()) } ; ($ arg_0 : expr , $ tail_0 : expr) => { f ($ arg_0 , :: core :: convert :: From :: from ($ tail_0)) } ; ($ arg_0 : expr , $ tail_0 : tt) => { f ($ arg_0 , :: bobtail :: __bobtail_handle_underscore ! ($ tail_0)) } ; }"#;
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
        
        // Expected output: impl block + explicit macro_rules! with match arms
        let expected = r#"impl A { pub fn b (& self , a : u8 , b : Option < u8 >) -> u8 { b . map (| x | x + a) . unwrap_or (a) } } # [macro_export] macro_rules ! b { ($ self_ : expr , $ arg_0 : expr) => { $ self_ . b ($ arg_0 , :: core :: default :: Default :: default ()) } ; ($ self_ : expr , $ arg_0 : expr , $ tail_0 : expr) => { $ self_ . b ($ arg_0 , :: core :: convert :: From :: from ($ tail_0)) } ; }"#;
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
        
        // Expected output: explicit match arms for each argument combination
        // Format: ($arg:expr) => { f($arg, Default::default()) } for 0 tail args
        //         ($arg:expr, $tail:expr) => { f($arg, From::from($tail)) } for 1 tail arg
        let expected = r#"macro_rules ! f { ($ arg_0 : expr) => { f ($ arg_0 , :: core :: default :: Default :: default ()) } ; ($ arg_0 : expr , $ tail_0 : expr) => { f ($ arg_0 , :: core :: convert :: From :: from ($ tail_0)) } ; }"#;
        assert_eq!(output_str.trim(), expected.trim());
    }
}
