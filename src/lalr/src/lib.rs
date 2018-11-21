#![crate_type = "dylib"]
#![feature(plugin_registrar, rustc_private)]

extern crate rustc;
extern crate rustc_plugin;
extern crate smallvec;
extern crate syntax;
extern crate syntax_pos;

use std::iter::Peekable;
use std::slice::Iter;

use generator::{compute_lalr, Rule, RuleData, RuleTranslationMap};
use rustc_plugin::Registry;
use smallvec::SmallVec;
use syntax::ast;
use syntax::ext::base::{DummyResult, ExtCtxt, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::Span;
use syntax::parse::token;
use syntax::ptr::P;
use syntax::tokenstream::TokenTree;

mod generator;

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

enum ParseResult {
    Success(Rule),
    Failure(Option<Span>),
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

fn parse_item(cx: &mut ExtCtxt, outer_span: Span,
              iter: &mut Peekable<Iter<'_, TokenTree>>,
              tm: &mut RuleTranslationMap) -> ParseResult {
    let mut rsp = outer_span;
    let mut terminal = false;
    let mut indirect = false;
    let (s, t) = match iter.next() {
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
        tt => return parse_failure(cx, s.to(rsp), tt)
    }

    let mut components: Vec<(String, String, Span)> = Vec::new();

    while !is_semi(iter.peek()) {
        match iter.next() {
            Some(TokenTree::Token(s, token::Ident(t, _))) => {
                rsp = *s;
                let name = t.name.to_string();
                components.push((name.clone(), name, *s));
            }
            Some(TokenTree::Token(s, token::ModSep)) => {
                rsp = *s;
                if let Some((_, mut c, cs)) = components.pop() {
                    match iter.next() {
                        Some(TokenTree::Token(s, token::Ident(tt, _))) => {
                            rsp = *s;
                            let right = tt.name.to_string();
                            c.push_str("::");
                            c.push_str(&right);
                            components.push((right, c, cs.to(*s)));
                        }
                        tt => return parse_failure(cx, s.to(rsp), tt)
                    }
                } else {
                    return parse_failure(cx, s.to(rsp), None);
                }
            }
            Some(TokenTree::Token(s, token::Pound)) => {
                rsp = *s;
                terminal = true;
            }
            Some(TokenTree::Token(s, token::BinOp(token::BinOpToken::And))) => {
                rsp = *s;
                indirect = true;
            }
            tt => return parse_failure(cx, s.to(rsp), tt)
        }
    }

    assert!(is_semi_r(iter.next()));

    let identifier = t.name.to_string();
    let span = s.to(rsp);

    let rule = if terminal {
        Rule {
            identifier,
            span,
            data: RuleData::Terminal,
        }
    } else {
        Rule {
            identifier,
            span,
            data: RuleData::Nonterminal {
                components,
                indirect,
            },
        }
    };

    if tm.push_rule(rule.identifier.clone(), rule.clone()).is_none() {
        cx.span_err(s.to(rsp), "Duplicate rule");
        return ParseResult::Failure(Some(s.to(rsp)));
    }

    ParseResult::Success(rule)
}

fn expand_rn(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
             -> Box<MacResult + 'static> {
    let mut tm = RuleTranslationMap { ..Default::default() };

    let mut iter = args.iter().peekable();

    let mut items = SmallVec::<[P<ast::Item>; 1]>::new();
    let mut parser_items = Vec::new();

    while iter.peek().is_some() {
        match parse_item(cx, sp, &mut iter, &mut tm) {
            ParseResult::Success(item) => {
                parser_items.push(item.clone());
                let indirect = match item.data {
                    RuleData::Nonterminal {indirect, ..}  => indirect,
                    RuleData::Terminal => false
                };
                let components = match item.data {
                    RuleData::Nonterminal {components, ..}  => components,
                    RuleData::Terminal => Vec::new()
                };
                let enumdef = ast::EnumDef {
                    variants: components
                        .into_iter()
                        .map(|item| {
                            let (n, x, s) = item;
                            let mut vals = Vec::new();
                            let ident_arr = x.rsplitn(2, "::");
                            // .unwrap() here is always safe
                            let ident_ty = cx.ty_ident(s, cx.ident_of(ident_arr.last().unwrap()));
                            let ty;
                            if indirect {
                                ty = cx.ty_path(
                                    cx.path_all(s, true,
                                                cx.std_path(&["boxed", "Box"]),
                                                vec![ast::GenericArg::Type(ident_ty)],
                                                Vec::new()));
                            } else {
                                ty = ident_ty;
                            }
                            vals.push(ty);
                            cx.variant(sp, cx.ident_of(&n), vals)
                        })
                        .collect()
                };
                let ident = cx.ident_of(&item.identifier);
                items.push(cx.item_enum(item.span, ident, enumdef));
            }
            ParseResult::Failure(span) => return DummyResult::any(span.unwrap_or(sp))
        }
    }

    return compute_lalr(tm, parser_items, items);
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("lalr", expand_rn);
}
