#![forbid(unsafe_code)]

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Error, ImplItem, ImplItemMethod,
    punctuated::Punctuated, Item, Lit, Meta, MetaList, MetaNameValue, NestedMeta, Pat, PatIdent,
    Path, Result, Token,
};

fn tail_optional_macros_path() -> Result<proc_macro2::TokenStream> {
    let found = crate_name("tail_optional_macros")
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

// (intentionally removed older impl-level parsing helpers during the split to block+method attributes)

fn strip_tail_omittable_attr(attrs: &mut Vec<Attribute>) {
    attrs.retain(|a| !a.path.is_ident("tail_omittable"));
}

fn strip_block_attr(attrs: &mut Vec<Attribute>) {
    attrs.retain(|a| !a.path.is_ident("block"));
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

/// Method marker/config attribute.
///
/// This attribute is intentionally a no-op (it returns the method unchanged), and is consumed by
/// `#[tail_omittable::block]` on the surrounding `impl`.
#[proc_macro_attribute]
pub fn tail_omittable(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut method: ImplItemMethod = parse_macro_input!(item as ImplItemMethod);
    strip_tail_omittable_attr(&mut method.attrs);
    quote!(#method).into()
}

/// `impl`-block attribute that generates `macro_rules!` wrappers for methods marked with `#[tail_omittable(..)]`.
///
/// Example:
/// ```rust,ignore
/// use tail_optional_macros as tail_omittable; // to use #[tail_omittable::block]
/// use tail_optional_macros::tail_omittable;  // to use #[tail_omittable]
///
/// #[tail_omittable::block]
/// impl Pico8 {
///     #[tail_omittable(conv(color(PColor::from)))]
///     fn sset(&mut self, pos: (u32,u32), color: Option<PColor>, sheet_index: Option<usize>) -> Result<(),()> { Ok(()) }
/// }
/// ```
#[proc_macro_attribute]
pub fn block(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_ts: proc_macro2::TokenStream = item.clone().into();

    let crate_path = match tail_optional_macros_path() {
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

                // Find `#[tail_omittable(...)]` marker on this method.
                let mut found_attr: Option<Attribute> = None;
                for a in &method_fn.attrs {
                    if a.path.is_ident("tail_omittable") {
                        found_attr = Some(a.clone());
                        break;
                    }
                }
                let Some(attr) = found_attr else { continue };

                let args: Vec<NestedMeta> = match parse_attr_args(&attr) {
                    Ok(v) => v,
                    Err(e) => return e.to_compile_error().into(),
                };

                let mut spec = MethodSpec::new(method_fn.sig.ident.clone());
                spec.required = Some(1); // default: first non-receiver param required, rest omittable

                for nm in &args {
                    match nm {
                        NestedMeta::Meta(Meta::Path(p)) => {
                            if let Some(id) = p.get_ident() {
                                spec.macro_name = Some(id.clone());
                            } else {
                                return Error::new(p.span(), "expected identifier").to_compile_error().into();
                            }
                        }
                        NestedMeta::Meta(Meta::NameValue(MetaNameValue { path, lit, .. })) => {
                            let key = match path.get_ident() {
                                Some(id) => id.to_string(),
                                None => return Error::new(path.span(), "expected identifier key").to_compile_error().into(),
                            };
                            match key.as_str() {
                                "name" | "macro" | "macro_name" => {
                                    let Lit::Str(s) = lit else {
                                        return Error::new(lit.span(), "expected string literal").to_compile_error().into();
                                    };
                                    spec.macro_name = Some(Ident::new(&s.value(), s.span()));
                                }
                                "required" => {
                                    let Lit::Int(n) = lit else {
                                        return Error::new(lit.span(), "expected integer literal").to_compile_error().into();
                                    };
                                    spec.required = Some(match n.base10_parse::<usize>() {
                                        Ok(v) => v,
                                        Err(e) => return Error::new(lit.span(), e.to_string()).to_compile_error().into(),
                                    });
                                }
                                _ => {
                                    return Error::new(path.span(), "unknown option (expected name/macro_name or required)")
                                        .to_compile_error()
                                        .into();
                                }
                            }
                        }
                        NestedMeta::Meta(Meta::List(MetaList { path, nested, .. })) => {
                            let key = match path.get_ident() {
                                Some(id) => id.to_string(),
                                None => return Error::new(path.span(), "expected identifier key").to_compile_error().into(),
                            };
                            match key.as_str() {
                                "conv" | "convert" => {
                                    // conv(color(PColor::from), other(path::to::conv))
                                    for n in nested.iter() {
                                        match n {
                                            NestedMeta::Meta(Meta::List(MetaList { path, nested, .. })) => {
                                                let arg = match path.get_ident() {
                                                    Some(id) => id.clone(),
                                                    None => return Error::new(path.span(), "expected parameter identifier").to_compile_error().into(),
                                                };
                                                if nested.len() != 1 {
                                                    return Error::new(n.span(), "expected exactly one path, e.g. color(PColor::from)")
                                                        .to_compile_error()
                                                        .into();
                                                }
                                                let NestedMeta::Meta(Meta::Path(conv_path)) = &nested[0] else {
                                                    return Error::new(nested[0].span(), "expected a path like PColor::from")
                                                        .to_compile_error()
                                                        .into();
                                                };
                                                spec.conv.push((arg, conv_path.clone()));
                                            }
                                            NestedMeta::Meta(Meta::NameValue(MetaNameValue { path, lit, .. })) => {
                                                // Back-compat: conv(color = "PColor::from")
                                                let arg = match path.get_ident() {
                                                    Some(id) => id.clone(),
                                                    None => return Error::new(path.span(), "expected parameter identifier").to_compile_error().into(),
                                                };
                                                let Lit::Str(s) = lit else {
                                                    return Error::new(lit.span(), "expected string literal path, e.g. \"PColor::from\"")
                                                        .to_compile_error()
                                                        .into();
                                                };
                                                let conv_path: Path = match syn::parse_str(&s.value()) {
                                                    Ok(p) => p,
                                                    Err(e) => return Error::new(lit.span(), e.to_string()).to_compile_error().into(),
                                                };
                                                spec.conv.push((arg, conv_path));
                                            }
                                            other => {
                                                return Error::new(other.span(), "expected color(PColor::from) or color = \"PColor::from\"")
                                                    .to_compile_error()
                                                    .into();
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    return Error::new(path.span(), "unknown list option (expected conv)")
                                        .to_compile_error()
                                        .into();
                                }
                            }
                        }
                        other => {
                            return Error::new(other.span(), "unsupported tail_omittable syntax")
                                .to_compile_error()
                                .into();
                        }
                    }
                }

                let sig_args = typed_args_after_receiver(&method_fn.sig);
                let receiver = receiver_tokens(&method_fn.sig);
                match generate_define_for_method(crate_path.clone(), receiver, &sig_args, &spec.method, &spec) {
                    Ok(ts) => generated.push(ts),
                    Err(e) => return e.to_compile_error().into(),
                }
            }

            strip_block_attr(&mut item_impl.attrs);

            let out = quote! {
                #item_impl
                #(#generated)*
            };
            out.into()
        }
        other => Error::new(other.span(), "tail_omittable::block can only be used on an impl block")
            .to_compile_error()
            .into(),
    }
}


