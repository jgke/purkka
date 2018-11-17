#![crate_type = "dylib"]
#![feature(plugin_registrar, rustc_private)]

extern crate rustc;
extern crate rustc_plugin;
extern crate smallvec;
extern crate syntax;
extern crate syntax_pos;

use rustc_plugin::Registry;
use smallvec::SmallVec;
use syntax::ast;
use syntax::ext::base::{DummyResult, ExtCtxt, MacEager, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::Span;
use syntax::parse::token;
use syntax::ptr::P;
use syntax::tokenstream::TokenTree;
use syntax_pos::DUMMY_SP;

use std::iter::Peekable;
use std::slice::Iter;

fn is_semi_r(tree: Option<&TokenTree>) -> bool {
    match tree {
        Some(TokenTree::Token(_, token::Semi)) => true,
        _ => false
    }
}

fn is_semi(tree: Option<&&TokenTree>) -> bool {
    match tree {
        Some(TokenTree::Token(_, token::Semi)) => true,
        _ => false
    }
}

struct Rule {
    identifier: String,
    components: Vec<String>,
    terminal: bool,
    span: Span
}

enum ParseResult {
    Success(Rule),
    Failure(Option<Span>)
}

fn parse_failure(cx: &mut ExtCtxt, outer_span: Span, tt: Option<&TokenTree>) -> ParseResult {
    match tt {
        Some(TokenTree::Token(span, t)) => {
            let s: Span = *span;
            cx.span_err(s, &format!("Unexpected token: {:?}", t));
            return ParseResult::Failure(Some(s));
        }
        Some(TokenTree::Delimited(span, t)) => {
            let s: Span = span.entire();
            cx.span_err(s, &format!("Unexpected span: {:?}", t));
            return ParseResult::Failure(Some(s));
        }
        None => {
            cx.span_err(outer_span, "Unexpected end of input");
            return ParseResult::Failure(None);
        }
    }
}

fn parse_item(cx: &mut ExtCtxt, outer_span: Span, iter: &mut Peekable<Iter<'_, TokenTree>>) -> ParseResult {
    let mut rsp = DUMMY_SP;
    let ident = match iter.next() {
        Some(TokenTree::Token(s, token::Ident(t, _))) => (s, t),
        tt => return parse_failure(cx, outer_span, tt)
    };
    match iter.next() {
        Some(TokenTree::Token(_, token::RArrow)) => {}
        Some(TokenTree::Token(span, _)) => {
            let s: Span = *span;
            cx.span_err(s, "Rule name must be followed by ->");
            return ParseResult::Failure(Some(s));
        }
        tt => return parse_failure(cx, outer_span, tt)
    }

    let mut components = Vec::new();

    while !is_semi(iter.peek()) {
        match iter.next() {
            Some(TokenTree::Token(s, token::Ident(t, _))) => {
                rsp = *s;
                components.push(t.name.to_string());
            }
            tt => return parse_failure(cx, outer_span, tt)
        }
    }

    assert!(is_semi_r(iter.next()));

    let (s, t) = ident;

    ParseResult::Success(Rule {identifier: t.name.to_string(), components, terminal: false , span: s.to(rsp) })
}

fn expand_rn(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
             -> Box<MacResult + 'static> {
    let mut iter = args.iter().peekable();

    let mut items = SmallVec::<[P<ast::Item>; 1]>::new();

    while iter.peek().is_some() {

        match parse_item(cx, sp, &mut iter) {
            ParseResult::Success(item) => {
                let enumdef = ast::EnumDef { variants: Vec::new() };
                let ident = cx.ident_of(&item.identifier);
                items.push(cx.item_enum(item.span, ident, enumdef));
            }
            ParseResult::Failure(span) => return DummyResult::any(span.unwrap_or(sp))
        }
    }

    MacEager::items(items)
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("lalr", expand_rn);
}
