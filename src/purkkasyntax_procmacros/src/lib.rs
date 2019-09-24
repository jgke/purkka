extern crate proc_macro;

use proc_macro2::TokenStream;
use syn::{parse_macro_input, Token, Result, Ident, LitInt, LitStr, Type};
use syn::parse::{Parse, ParseStream};
use quote::ToTokens;

struct Options {
    fmap: bool,
    unbox: bool,
}

fn format_blocks(
    options: Options,
    this: &Ident,
    variant: &Ident,
    result_type: &Type,
    fn_name: &Ident,
    field_num: usize,
) -> TokenStream {
    let pat = format!(
        "{}::{}({} t, ..)",
        this,
        variant,
        std::iter::repeat("_, ".to_string())
            .take(field_num - 1)
            .collect::<String>()
    );
    let trait_name = format!("{}_{}", this, fn_name);
    let t = match (options.fmap, options.unbox) {
        (false, false) => "Some(t)",
        (true, false) => "t.as_ref()",
        (false, true) => "Some(t.as_ref())",
        (true, true) => "t.as_ref().map(AsRef::as_ref)",
    };
    let impl_enter_match = format!("if let Some({}) = self {{ {} }} else {{ None }}", pat, t);

    let trait_block = format!(
        "
pub trait {}<'a> {{
        fn {}(&self) -> Option<&'a {}>;
}}
",
        trait_name, fn_name, result_type.to_token_stream().to_string()
    );

    let impl_block = format!(
        "
impl<'a> {}<'a> for Option<&'a {}> {{
    fn {}(&self) -> Option<&'a {}> {{
        {}
    }}
}}
",
        trait_name, this, fn_name, result_type.to_token_stream().to_string(), impl_enter_match
    );

    format!("{}{}", trait_block, impl_block).parse().unwrap()
}

fn impl_enter_res(
    args: ParseStream,
    options: Options,
) -> Result<TokenStream> {
    let this = args.parse::<Ident>()?;
    args.parse::<Token![,]>()?;
    let variant = args.parse::<Ident>()?;
    args.parse::<Token![,]>()?;
    let result_type: Type = if args.peek(Ident) {
        args.parse()?
    } else {
        args.parse::<LitStr>()?.parse()?
    };
    args.parse::<Token![,]>()?;
    let fn_name = args.parse::<Ident>()?;
    args.parse::<Token![,]>()?;
    let field_num = args.parse::<LitInt>()?.base10_parse()?;
    Ok(format_blocks(
        options,
        &this,
        &variant,
        &result_type,
        &fn_name,
        field_num
    ))
}

struct M { result: TokenStream }
struct Mb { result: TokenStream }
struct Mf { result: TokenStream }
struct Mfb { result: TokenStream }

impl Parse for M {
    fn parse(buf: ParseStream) -> Result<Self> {
        let result = impl_enter_res(buf, Options { fmap: false, unbox: false })?;
        Ok(M { result })
    }
}

impl Parse for Mb {
    fn parse(buf: ParseStream) -> Result<Self> {
        let result = impl_enter_res(buf, Options { fmap: false, unbox: true })?;
        Ok(Mb { result })
    }
}

impl Parse for Mf {
    fn parse(buf: ParseStream) -> Result<Self> {
        let result = impl_enter_res(buf, Options { fmap: true, unbox: false })?;
        Ok(Mf { result })
    }
}

impl Parse for Mfb {
    fn parse(buf: ParseStream) -> Result<Self> {
        let result = impl_enter_res(buf, Options { fmap: true, unbox: true })?;
        Ok(Mfb { result })
    }
}

#[proc_macro]
pub fn impl_enter(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from((parse_macro_input!(input as M)).result)
}

#[proc_macro]
pub fn impl_enter_fmap(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from((parse_macro_input!(input as Mf)).result)
}

#[proc_macro]
pub fn impl_enter_unbox(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from((parse_macro_input!(input as Mb)).result)
}

#[proc_macro]
pub fn impl_enter_unbox_fmap(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from((parse_macro_input!(input as Mfb)).result)
}
