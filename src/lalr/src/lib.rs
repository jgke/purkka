#![crate_type="dylib"]
#![feature(plugin_registrar, rustc_private)]

extern crate syntax;
extern crate syntax_pos;
extern crate rustc;
extern crate rustc_plugin;
extern crate smallvec;

use syntax::parse::token;
use syntax::ast;
use syntax::tokenstream::TokenTree;
use syntax::ext::base::{ExtCtxt, MacResult, DummyResult, MacEager};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::Span;
use syntax_pos::DUMMY_SP;
use rustc_plugin::Registry;
use smallvec::SmallVec;
use syntax::ptr::P;

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

fn expand_rn(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
        -> Box<MacResult + 'static> {
    let mut iter = args.iter().peekable();

    let mut items = SmallVec::<[P<ast::Item>; 1]>::new();

    while iter.peek().is_some() {
        let mut rsp = DUMMY_SP;
        let ident = match iter.next() {
            Some(TokenTree::Token(s, token::Ident(t, _))) => (s, t),
            _ => {
                cx.span_err(sp, "Rule name must be an identifier");
                return DummyResult::any(sp);
            }
        };
        match iter.next() {
            Some(TokenTree::Token(_, token::RArrow)) => {}
            _ => {
                cx.span_err(sp, "Rule name must be followed by ->");
                return DummyResult::any(sp);
            }
        }
        while !is_semi(iter.peek()) {
            match iter.next() {
                Some(TokenTree::Token(s, token::Ident(_, _))) => {
                    rsp = *s;
                }
                Some(TokenTree::Token(span, token)) => {
                    let s: Span = *span;
                    cx.span_err(s, &format!("Unexpected token: {:?}", token));
                    return DummyResult::any(s);
                }
                any => {
                    cx.span_err(sp, &format!("Unexpected token: {:?}", any));
                    return DummyResult::any(sp);
                }
            }
        }

        assert!(is_semi_r(iter.next()));

        let (s, t) = ident;

        items.push(cx.item_enum(s.to(rsp), *t, ast::EnumDef {variants: Vec::new()}));
    }

    MacEager::items(items)
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("lalr", expand_rn);
}
