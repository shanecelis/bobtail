#![forbid(unsafe_code)]

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::quote;
use syn::{
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    Attribute,
    Error,
    Ident,
    ImplItem,
    ImplItemMethod,
    Item,
    Meta,
    NestedMeta,
    Pat,
    Result,
    Token,
};

fn bobtail_path() -> Result<proc_macro2::TokenStream> {
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

#[derive(Debug)]
struct MethodSpec {
    method: Ident,
    macro_name: Option<Ident>,
    // conv: Vec<(Ident, Path)>,
}

impl MethodSpec {
    fn new(method: Ident) -> Self {
        Self { method, macro_name: None, //conv: Vec::new()
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

/// Method marker/config attribute (no-op).
#[proc_macro_attribute]
pub fn bob(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut method: ImplItemMethod = parse_macro_input!(item as ImplItemMethod);
    method.attrs.retain(|a| !is_bob_attr(a));
    quote!(#method).into()
}

/// `impl`-block attribute that generates `macro_rules!` wrappers for methods marked with `#[tail_omittable(..)]`.
#[proc_macro_attribute]
pub fn block(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_ts: proc_macro2::TokenStream = item.clone().into();

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

            for it in &item_impl.items {
                let ImplItem::Method(method_fn) = it else { continue };

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
                    return Error::new(method_fn.sig.span(), "bobtail::block currently requires a self receiver").to_compile_error().into();
                };

                // Build prototype params by largely reusing the method signature + markers.
                let mut proto_params: Vec<proc_macro2::TokenStream> = Vec::new();
                proto_params.push(quote!(#receiver,));

                // Track the first #[bobtail::tail] to emit #[tail] exactly once.
                let mut tail_started = false;

                for arg in method_fn.sig.inputs.iter().skip(1) {
                    let syn::FnArg::Typed(pat_ty) = arg else { continue };
                    let Pat::Ident(pat_ident) = pat_ty.pat.as_ref() else {
                        return Error::new(pat_ty.pat.span(), "bobtail only supports ident parameters").to_compile_error().into();
                    };
                    let name = &pat_ident.ident;
                    let ty = pat_ty.ty.as_ref();

                    let has_tail = pat_ty.attrs.iter().any(is_tail_attr);
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

                let out_ty = match &method_fn.sig.output {
                    syn::ReturnType::Default => None,
                    syn::ReturnType::Type(_, ty) => Some(ty.as_ref()),
                };

                // Emit as a tail_define item (method prototype).
                let method = &spec.method;
                if let Some(mac) = &spec.macro_name {
                    if let Some(ret) = out_ty {
                        items.push(quote!(#mac => fn #method( #(#proto_params)* ) -> #ret;));
                    } else {
                        items.push(quote!(#mac => fn #method( #(#proto_params)* );));
                    }
                } else if let Some(ret) = out_ty {
                    items.push(quote!(fn #method( #(#proto_params)* ) -> #ret;));
                } else {
                    items.push(quote!(fn #method( #(#proto_params)* );));
                }
            }

            // Strip marker attrs from the output impl so the compiler never has to resolve them.
            for it in &mut item_impl.items {
                let ImplItem::Method(method_fn) = it else { continue };
                method_fn.attrs.retain(|a| !is_bob_attr(a));
                for arg in &mut method_fn.sig.inputs {
                    let syn::FnArg::Typed(pat_ty) = arg else { continue };
                    pat_ty.attrs.retain(|a| !is_tail_attr(a) && !is_map_attr(a));
                }
            }

            strip_block_attr(&mut item_impl.attrs);

            let tail_define_block = if items.is_empty() {
                quote!()
            } else {
                quote!(#crate_path::tail_define!( #(#items)* );)
            };

            let out = quote! {
                #item_impl
                #tail_define_block
            };
            out.into()
        }
        other => Error::new(other.span(), "bobtail::block can only be used on an impl block")
            .to_compile_error()
            .into(),
    }
}

// ---- tail_define proc macro (prototype -> macro_rules wrapper generator) ----

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
                        return Err(Error::new(fn_name.span(), "receiver must be the first parameter"));
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

fn tail_define_impl(input: TokenStream) -> TokenStream {
    let crate_path = match bobtail_path() {
        Ok(p) => p,
        Err(e) => return e.to_compile_error().into(),
    };

    let parsed: TailDefineInput = parse_macro_input!(input as TailDefineInput);
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

        out.extend(quote! {
            #(#outer_attrs)*
            macro_rules! #macro_name {
                ($($call:tt)*) => {
                    #crate_path::__tail_omittable_munch!(
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

    out.into()
}

#[proc_macro]
pub fn tail_define(input: TokenStream) -> TokenStream {
    tail_define_impl(input)
}

#[proc_macro]
pub fn define_tail_optional_macro(input: TokenStream) -> TokenStream {
    tail_define_impl(input)
}

#[proc_macro]
pub fn define_tail(input: TokenStream) -> TokenStream {
    tail_define_impl(input)
}


