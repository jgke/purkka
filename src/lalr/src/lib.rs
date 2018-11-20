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

use std::collections::HashMap;
use std::iter::Peekable;
use std::rc::Rc;
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

#[derive(Default)]
struct RuleTranslationMap {
    rule_to_number: HashMap<Rc<String>, i64>,
    number_to_rule: HashMap<i64, Rc<String>>,
    rule_number: i64
}

impl RuleTranslationMap {
    fn push_rule(&mut self, rule: String) -> bool {
        if(self.rule_to_number.get(&rule).is_some()) {
            return false;
        }

        let num = self.rule_number;
        let r = Rc::new(rule);
        self.rule_number += 1;
        self.rule_to_number.insert(Rc::clone(&r), num);
        self.number_to_rule.insert(num, r);
        return true;
    }
}

struct Rule {
    identifier: String,
    components: Vec<(String, String, Span)>,
    terminal: bool,
    indirect: bool,
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
                            components.push((right, c, *s));
                        }
                        tt => return parse_failure(cx, s.to(rsp), tt)
                    }
                } else {
                    return parse_failure(cx, s.to(rsp), None)
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

    if !tm.push_rule(identifier.clone()) {
        cx.span_err(s.to(rsp), "Duplicate rule");
        return ParseResult::Failure(Some(s.to(rsp)));
    }

    ParseResult::Success(Rule {
        identifier,
        components,
        terminal,
        indirect,
        span: s.to(rsp) })
}

fn expand_rn(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
             -> Box<MacResult + 'static> {
    let mut tm = RuleTranslationMap { ..Default::default() } ;

    let mut iter = args.iter().peekable();

    let mut items = SmallVec::<[P<ast::Item>; 1]>::new();

    while iter.peek().is_some() {

        match parse_item(cx, sp, &mut iter, &mut tm) {
            ParseResult::Success(item) => {
                let indirect = item.indirect;
                let enumdef = ast::EnumDef {
                    variants: item.components
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
                                    cx.path_all(s,
                                                  true,
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

    println!("{:?}", tm.rule_to_number);
    println!("{:?}", tm.number_to_rule);

    MacEager::items(items)
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("lalr", expand_rn);
}
