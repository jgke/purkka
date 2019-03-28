#![crate_type = "dylib"]
#![feature(plugin_registrar, rustc_private)]

extern crate rustc;
extern crate rustc_plugin;
extern crate rustc_target;
extern crate smallvec;
extern crate syntax;
extern crate syntax_pos;

use rustc_plugin::Registry;
use syntax::ext::base::{DummyResult, ExtCtxt, MacResult};
use syntax_pos::Span;
use syntax::parse::token;
use syntax::tokenstream::TokenTree;

use std::collections::HashSet;
use std::iter::Peekable;
use std::slice::Iter;

mod ast_output;
mod generator;
mod types;

use ast_output::output_parser;
use generator::compute_lalr;
use types::{Component, Rule, RuleData, RuleTranslationMap, Terminal};

fn is_semi_r(tree: Option<&TokenTree>) -> bool {
    match tree {
        Some(TokenTree::Token(_, token::Semi)) => true,
        _ => false,
    }
}

fn is_semi(tree: Option<&&TokenTree>) -> bool {
    match tree {
        Some(TokenTree::Token(_, token::Semi)) => true,
        _ => false,
    }
}

enum ParseResult<SuccessType> {
    Success(SuccessType),
    Failure(Option<Span>),
}

fn parse_failure<SuccessType>(cx: &mut ExtCtxt, outer_span: Span, tt: Option<&TokenTree>) -> ParseResult<SuccessType> {
    match tt {
        Some(TokenTree::Token(span, t)) => {
            let s: Span = *span;
            cx.span_err(s, &format!("Unexpected token: {:?}", t));
            return ParseResult::Failure(Some(s));
        }
        Some(TokenTree::Delimited(span, _, t)) => {
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
fn parse_special(
    cx: &mut ExtCtxt,
    outer_span: Span,
    iter: &mut Peekable<Iter<'_, TokenTree>>,
) -> ParseResult<Terminal> {
    let mut rsp = outer_span;
    let s = match iter.next() {
        Some(TokenTree::Token(s, token::Not)) => s,
        tt => return parse_failure(cx, outer_span, tt),
    };
    let rule_name = match iter.next() {
        Some(TokenTree::Token(_, token::Ident(t, _))) => t,
        tt => return parse_failure(cx, *s, tt),
    };
    match iter.next() {
        Some(TokenTree::Token(_, token::RArrow)) => {}
        Some(TokenTree::Token(span, _)) => {
            let s: Span = *span;
            cx.span_err(s, "Special rule name must be followed by ->");
            return ParseResult::Failure(Some(s));
        }
        tt => return parse_failure(cx, *s, tt),
    }

    let function_name = match iter.next() {
        Some(TokenTree::Token(_, token::Ident(tt, _))) => tt.name.to_string(),
        tt => return parse_failure(cx, *s, tt),
    };

    match iter.next() {
        Some(TokenTree::Token(_, token::Pound)) => {}
        tt => return parse_failure(cx, *s, tt),
    };

    let mut special_component = match iter.next() {
        Some(TokenTree::Token(s, token::Ident(tt, _))) => {
            rsp = *s;
            Terminal {
                identifier: tt.name.to_string(),
                full_path: tt.name.to_string(),
                span: s.to(rsp),
                conversion_fn: None
            }
        }
        tt => return parse_failure(cx, s.to(rsp), tt)
    };

    while !is_semi(iter.peek()) {
        match iter.next() {
            Some(TokenTree::Token(s, token::ModSep)) => {
                rsp = *s;
                match iter.next() {
                    Some(TokenTree::Token(s, token::Ident(tt, _))) => {
                        rsp = *s;
                        let right = tt.name.to_string();
                        special_component.full_path.push_str("::");
                        special_component.full_path.push_str(&right);
                        special_component.identifier = right;
                        special_component.span = special_component.span.to(*s);
                    }
                    tt => return parse_failure(cx, s.to(rsp), tt),
                }
            }
            tt => return parse_failure(cx, s.to(rsp), tt),
        }
    }

    special_component.conversion_fn = Some((function_name, special_component.full_path));
    special_component.identifier = rule_name.to_string();
    special_component.full_path = rule_name.to_string();

    assert!(is_semi_r(iter.next()));

    ParseResult::Success(special_component)
}

fn parse_item(
    cx: &mut ExtCtxt,
    outer_span: Span,
    iter: &mut Peekable<Iter<'_, TokenTree>>,
    tm: &mut RuleTranslationMap,
) -> ParseResult<Rule> {
    let mut rsp = outer_span;
    let mut terminal = false;
    let mut indirect = false;
    let (s, t) = match iter.next() {
        Some(TokenTree::Token(s, token::Ident(t, _))) => (s, t),
        tt => return parse_failure(cx, outer_span, tt),
    };
    match iter.next() {
        Some(TokenTree::Token(_, token::RArrow)) => {}
        Some(TokenTree::Token(span, _)) => {
            let s: Span = *span;
            cx.span_err(s, "Rule name must be followed by ->");
            return ParseResult::Failure(Some(s));
        }
        tt => return parse_failure(cx, s.to(rsp), tt),
    }

    let mut components: Vec<Component> = Vec::new();
    let mut real_name: Option<String> = None;
    let mut current_components: Vec<RuleData> = Vec::new();
    let mut action = None;
    let mut priority: Option<(i32, bool)> = None;

    while !is_semi(iter.peek()) {
        match iter.next() {
            Some(TokenTree::Token(_, token::Literal(token::Integer(new_prio), None))) => {
                priority = match iter.next() {
                    Some(TokenTree::Token(_, token::Colon)) =>
                        Some((new_prio.to_string().parse().unwrap(), true)),
                    Some(TokenTree::Token(_, token::Ident(tt, _))) => {
                        match (iter.next(), tt.name.to_string().as_ref()) {
                            (Some(TokenTree::Token(_, token::Colon)), "l") =>
                                Some((new_prio.to_string().parse().unwrap(), true)),
                            (Some(TokenTree::Token(_, token::Colon)), "r") =>
                                Some((new_prio.to_string().parse().unwrap(), false)),
                            (tt, _) => return parse_failure(cx, s.to(rsp), tt)
                        }

                    }
                    tt => return parse_failure(cx, s.to(rsp), tt)
                };
            }
            Some(TokenTree::Token(_, token::Not)) => {
                action = match iter.next() {
                    Some(TokenTree::Token(_, token::Ident(tt, _))) =>
                        Some(tt.name.to_string()),
                    tt => return parse_failure(cx, s.to(rsp), tt)
                };
            }
            Some(TokenTree::Token(s, token::Ident(tt, _))) => {
                rsp = *s;
                current_components.push(RuleData {
                    identifier: tt.name.to_string(),
                    full_path: tt.name.to_string(),
                    span: rsp,
                    terminal,
                    indirect: indirect || tt.name.as_str() == t.name.as_str(),
                    conversion_fn: None
                });
                terminal = false;
                indirect = false;
            }
            Some(TokenTree::Token(s, token::ModSep)) => {
                rsp = *s;
                if let Some(mut data) = current_components.pop() {
                    match iter.next() {
                        Some(TokenTree::Token(s, token::Ident(tt, _))) => {
                            rsp = *s;
                            let right = tt.name.to_string();
                            data.full_path.push_str("::");
                            data.full_path.push_str(&right);
                            data.identifier = right;
                            data.span = data.span.to(*s);
                            current_components.push(data);
                        }
                        tt => return parse_failure(cx, s.to(rsp), tt),
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
            Some(TokenTree::Token(s, token::BinOp(token::BinOpToken::Or))) => {
                rsp = *s;
                let real_real_name = real_name
                    .unwrap_or_else(|| current_components.get(0).unwrap().identifier.clone());
                real_name = None;
                components.push(Component {
                    real_name: real_real_name,
                    action,
                    rules: current_components,
                    priority
                });
                action = None;
                priority = None;
                current_components = vec![];
            }
            Some(TokenTree::Token(s, token::Dot)) => {
                rsp = *s;
                if let Some(data) = current_components.pop() {
                    real_name = Some(data.identifier);
                } else {
                    return parse_failure(cx, s.to(rsp), None);
                }
            }
            tt => return parse_failure(cx, s.to(rsp), tt),
        }
    }
    if current_components.len() > 0 {
        let real_real_name =
            real_name.unwrap_or_else(|| current_components.get(0).unwrap().identifier.clone());
        components.push(Component {
            real_name: real_real_name,
            action,
            rules: current_components,
            priority,
        });
    }

    assert!(is_semi_r(iter.next()));

    let identifier = t.name.as_str();
    let span = s.to(rsp);

    let rule = Rule {
        identifier: identifier.to_string(),
        span,
        data: components,
    };

    if tm
        .push_rule(rule.identifier.clone(), rule.clone())
        .is_none()
    {
        cx.span_err(s.to(rsp), "Duplicate rule (Hint: Mark the expression A -> B; A -> C; with A -> B | C;)");
        return ParseResult::Failure(Some(s.to(rsp)));
    }

    ParseResult::Success(rule)
}

fn expand_lalr(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
    let mut tm = RuleTranslationMap {
        ..Default::default()
    };
    let mut terminals: HashSet<Terminal> = HashSet::new();

    let mut iter = args.iter().peekable();

    let mut items = Vec::new();
    items.push(Rule {
        identifier: "Epsilon".to_string(),
        span: sp,
        data: vec![],
    });
    items.push(Rule {
        identifier: "$".to_string(),
        span: sp,
        data: vec![],
    });
    for rule in &items {
        tm.push_rule(rule.identifier.clone(), rule.clone());
    }

    loop {
        match iter.peek() {
            Some(TokenTree::Token(_, token::Not)) => match parse_special(cx, sp, &mut iter) {
                ParseResult::Success(item) => {
                    terminals.insert(item);
                }
                ParseResult::Failure(span) => return DummyResult::any(span.unwrap_or(sp)),
            }
            Some(_) => match parse_item(cx, sp, &mut iter, &mut tm) {
                ParseResult::Success(item) => {
                    item.data.iter().for_each(|rules| {
                        rules.rules.iter().filter(|x| x.terminal).for_each(|x| {
                            terminals.insert(Terminal {
                                identifier: x.identifier.clone(),
                                full_path: x.full_path.clone(),
                                span: sp,
                                conversion_fn: None
                            });
                        })
                    });

                    items.push(item);
                }
                ParseResult::Failure(span) => return DummyResult::any(span.unwrap_or(sp)),
            }
            None => break,
        }
    }

    let lalr_table = compute_lalr(&tm, &terminals);
    return output_parser(cx, sp, &tm, &items, &terminals, &lalr_table);
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("lalr", expand_lalr);
}
