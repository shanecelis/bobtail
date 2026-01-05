#![forbid(unsafe_code)]

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::quote;
use syn::{
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    Attribute,
    Error,
    ImplItem,
    ImplItemMethod,
    Item,
    Lit,
    Meta,
    MetaList,
    MetaNameValue,
    NestedMeta,
    Pat,
    PatIdent,
    Path,
    Result,
    Token,
};

fn bobtail_path() -> Result<proc_macro2::TokenStream> {
    let found = crate_name("bobtail")
        .map_err(|e| Error::new(Span::call_site(), format!("proc-macro-crate error: {e}")))?;
    Ok(match found {
        FoundCrate::Itself => quote!(crate),
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
    required: Option<usize>,
    conv: Vec<(Ident, Path)>,
}

impl MethodSpec {
    fn new(method: Ident) -> Self {
        Self { method, macro_name: None, required: None, conv: Vec::new() }
    }
}

fn strip_tail_omittable_attr(attrs: &mut Vec<Attribute>) {
    attrs.retain(|a| !a.path.is_ident("tail_omittable"));
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

fn typed_args_after_receiver(sig: &syn::Signature) -> Vec<(&PatIdent, &syn::Type)> {
    sig.inputs
        .iter()
        .filter_map(|arg| match arg {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_ty) => {
                let Pat::Ident(pat_ident) = pat_ty.pat.as_ref() else { return None };
                Some((pat_ident, pat_ty.ty.as_ref()))
            }
        })
        .collect()
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

fn generate_define_for_method(
    crate_path: proc_macro2::TokenStream,
    receiver: Option<proc_macro2::TokenStream>,
    type_params: &[(&PatIdent, &syn::Type)],
    method: &Ident,
    spec: &MethodSpec,
) -> Result<proc_macro2::TokenStream> {
    let macro_name = spec.macro_name.clone().unwrap_or_else(|| spec.method.clone());
    let required = spec.required.unwrap_or(1);

    if type_params.len() < required {
        return Err(Error::new(
            method.span(),
            format!("required={required} but method has only {} non-receiver parameters", type_params.len()),
        ));
    }

    let (req, opt) = type_params.split_at(required);

    let mut conv_map = std::collections::HashMap::<String, Path>::new();
    for (arg, p) in &spec.conv {
        conv_map.insert(arg.to_string(), p.clone());
    }

    let mut proto_params: Vec<proc_macro2::TokenStream> = Vec::new();
    if let Some(rcv) = receiver {
        proto_params.push(quote!(#rcv,));
    }
    for (pat, ty) in req {
        let name = &pat.ident;
        proto_params.push(quote!(#name : #ty,));
    }
    if let Some(((first_pat, first_ty), rest)) = opt.split_first() {
        let name = &first_pat.ident;
        if let Some(conv) = conv_map.get(&name.to_string()) {
            proto_params.push(quote!(#[tail] #[map(#conv)] #name : #first_ty,));
        } else {
            proto_params.push(quote!(#[tail] #name : #first_ty,));
        }
        for (pat, ty) in rest {
            let name = &pat.ident;
            if let Some(conv) = conv_map.get(&name.to_string()) {
                proto_params.push(quote!(#[map(#conv)] #name : #ty,));
            } else {
                proto_params.push(quote!(#name : #ty,));
            }
        }
    }

    Ok(quote! {
        #crate_path::define_tail_optional_macro!(
            #macro_name => fn #method(
                #(#proto_params)*
            );
        );
    })
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
            let mut generated = Vec::<proc_macro2::TokenStream>::new();

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

                // Determine required vs tail based on the first parameter marked `#[bobtail::tail]`.
                let mut tail_index: Option<usize> = None;
                let mut conv: Vec<(Ident, Path)> = Vec::new();
                let mut typed: Vec<(&PatIdent, &syn::Type)> = Vec::new();

                for (i, arg) in method_fn
                    .sig
                    .inputs
                    .iter()
                    .filter_map(|a| match a {
                        syn::FnArg::Receiver(_) => None,
                        syn::FnArg::Typed(p) => Some(p),
                    })
                    .enumerate()
                {
                    let Pat::Ident(pat_ident) = arg.pat.as_ref() else { continue };
                    typed.push((pat_ident, arg.ty.as_ref()));

                    if tail_index.is_none() && arg.attrs.iter().any(is_tail_attr) {
                        tail_index = Some(i);
                    }
                    for a in &arg.attrs {
                        if is_map_attr(a) {
                            let p: Path = match a.parse_args() {
                                Ok(p) => p,
                                Err(e) => return e.to_compile_error().into(),
                            };
                            conv.push((pat_ident.ident.clone(), p));
                        }
                    }
                }

                spec.required = Some(tail_index.unwrap_or(typed.len()));
                spec.conv = conv;

                let receiver = receiver_tokens(&method_fn.sig);
                match generate_define_for_method(crate_path.clone(), receiver, &typed, &spec.method, &spec) {
                    Ok(ts) => generated.push(ts),
                    Err(e) => return e.to_compile_error().into(),
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

            let out = quote! {
                #item_impl
                #(#generated)*
            };
            out.into()
        }
        other => Error::new(other.span(), "bobtail::block can only be used on an impl block")
            .to_compile_error()
            .into(),
    }
}


