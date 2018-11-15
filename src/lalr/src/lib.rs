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

        let mut variants = Vec::new();

        while !is_semi(iter.peek()) {
            match iter.next() {
                Some(TokenTree::Token(s, token::Ident(t, _))) => {
                    rsp = *s;
                    variants.push(cx.variant(*s, *t, Vec::new()));
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

        items.push(cx.item_enum(s.to(rsp), *t, ast::EnumDef { variants }));
    }

    MacEager::items(items)
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("lalr", expand_rn);
}
