#![crate_type = "dylib"]
#![feature(plugin_registrar, rustc_private)]

extern crate rustc;
extern crate rustc_driver;
extern crate rustc_target;
extern crate smallvec;
extern crate syntax;
extern crate syntax_pos;

use rustc_driver::plugin::Registry;

use syntax::ext::base::{DummyResult, ExtCtxt, MacEager, MacResult};
use syntax::parse;
use syntax::parse::token;

use syntax::tokenstream::TokenTree;
use syntax_pos::{FileName, Span};

struct Options {
    fmap: bool,
    unbox: bool,
}

fn format_blocks(
    cx: &mut ExtCtxt,
    options: Options,
    this: &str,
    variant: &str,
    result_type: &str,
    fn_name: &str,
    field_num: usize,
) -> Box<dyn MacResult + 'static> {
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
        trait_name, fn_name, result_type
    );

    let impl_block = format!(
        "
impl<'a> {}<'a> for Option<&'a {}> {{
    fn {}(&self) -> Option<&'a {}> {{
        {}
    }}
}}
",
        trait_name, this, fn_name, result_type, impl_enter_match
    );

    let session = cx.parse_sess();

    let filename = FileName::Custom(trait_name);
    let mut parser = parse::new_parser_from_source_str(
        session,
        filename,
        format!("{}{}", trait_block, impl_block),
    );

    let trait_item = parser.parse_item().unwrap().unwrap();
    let impl_item = parser.parse_item().unwrap().unwrap();
    MacEager::items(vec![trait_item, impl_item].into_iter().collect())
}

fn parse_failure(
    cx: &mut ExtCtxt,
    outer_span: Span,
    tt: Option<&TokenTree>,
) -> Box<dyn MacResult + 'static> {
    match tt {
        Some(TokenTree::Token(t)) => {
            let s: Span = t.span;
            cx.span_err(s, &format!("Unexpected token: {:?}", t));
            DummyResult::any(s)
        }
        Some(TokenTree::Delimited(span, _, t)) => {
            let s: Span = span.entire();
            cx.span_err(s, &format!("Unexpected block: {:?}", t));
            DummyResult::any(s)
        }
        None => DummyResult::any(outer_span),
    }
}

fn read_ident(
    cx: &mut ExtCtxt,
    sp: Span,
    iter: &mut std::slice::Iter<'_, TokenTree>,
) -> Result<String, Box<dyn MacResult + 'static>> {
    match iter.next() {
        Some(TokenTree::Token(token::Token {
            kind: token::TokenKind::Ident(t, _),
            ..
        })) => Ok(t.to_string()),
        tt => Err(parse_failure(cx, sp, tt)),
    }
}

fn read_comma(
    cx: &mut ExtCtxt,
    sp: Span,
    iter: &mut std::slice::Iter<'_, TokenTree>,
) -> Result<(), Box<dyn MacResult + 'static>> {
    match iter.next() {
        Some(TokenTree::Token(token::Token {
            kind: token::TokenKind::Comma,
            ..
        })) => Ok(()),
        tt => Err(parse_failure(cx, sp, tt)),
    }
}

fn impl_enter_res(
    cx: &mut ExtCtxt,
    sp: Span,
    args: &[TokenTree],
    options: Options,
) -> Result<Box<dyn MacResult + 'static>, Box<dyn MacResult + 'static>> {
    let mut iter = args.iter();
    let this = read_ident(cx, sp, &mut iter)?;
    read_comma(cx, sp, &mut iter)?;
    let variant = read_ident(cx, sp, &mut iter)?;
    read_comma(cx, sp, &mut iter)?;
    let result_type: String = match iter.next() {
        Some(TokenTree::Token(token::Token {
            kind: token::TokenKind::Ident(t, _),
            ..
        })) => t.to_string(),
        Some(TokenTree::Token(token::Token {
            kind:
                token::TokenKind::Literal(token::Lit {
                    kind: token::LitKind::Str,
                    symbol,
                    ..
                }),
            ..
        })) => symbol.to_string(),
        tt => return Err(parse_failure(cx, sp, tt)),
    };
    read_comma(cx, sp, &mut iter)?;
    let fn_name = read_ident(cx, sp, &mut iter)?;
    read_comma(cx, sp, &mut iter)?;
    let field_num = match iter.next() {
        Some(TokenTree::Token(token::Token {
            kind:
                token::TokenKind::Literal(token::Lit {
                    kind: token::LitKind::Integer,
                    symbol,
                    ..
                }),
            ..
        })) => symbol.to_string(),
        tt => return Err(parse_failure(cx, sp, tt)),
    };
    Ok(format_blocks(
        cx,
        options,
        &this,
        &variant,
        &result_type,
        &fn_name,
        field_num.parse().unwrap(),
    ))
}

fn impl_enter(
    cx: &mut ExtCtxt,
    sp: Span,
    args: &[TokenTree],
    options: Options,
) -> Box<dyn MacResult + 'static> {
    match impl_enter_res(cx, sp, args, options) {
        Ok(t) => t,
        Err(t) => t,
    }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("impl_enter", |cx, sp, args| {
        impl_enter(
            cx,
            sp,
            args,
            Options {
                fmap: false,
                unbox: false,
            },
        )
    });
    reg.register_macro("impl_enter_fmap", |cx, sp, args| {
        impl_enter(
            cx,
            sp,
            args,
            Options {
                fmap: true,
                unbox: false,
            },
        )
    });
    reg.register_macro("impl_enter_unbox", |cx, sp, args| {
        impl_enter(
            cx,
            sp,
            args,
            Options {
                fmap: false,
                unbox: true,
            },
        )
    });
    reg.register_macro("impl_enter_unbox_fmap", |cx, sp, args| {
        impl_enter(
            cx,
            sp,
            args,
            Options {
                fmap: true,
                unbox: true,
            },
        )
    });
}
