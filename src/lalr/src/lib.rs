#![crate_type = "dylib"]
#![feature(plugin_registrar, rustc_private)]

extern crate rustc;
extern crate rustc_driver;
extern crate rustc_target;
extern crate smallvec;
extern crate syntax;
extern crate syntax_pos;

use rustc_driver::plugin::Registry;
use syntax::ast;
use syntax::ext::base::{DummyResult, ExtCtxt, MacResult};
use syntax::parse;
use syntax::parse::token;
use syntax::ptr::P;
use syntax::tokenstream::TokenTree;
use syntax_pos::Span;

use std::collections::HashSet;
use std::iter::Peekable;
use std::slice::Iter;

mod ast_output;
mod types;

use ast_output::output_parser;
use types::{Component, Rule, RuleData, RuleTranslationMap, Terminal};

fn is_semi_r(tree: Option<&TokenTree>) -> bool {
    match tree {
        Some(TokenTree::Token(token::Token {
            kind: token::TokenKind::Semi,
            ..
        })) => true,
        _ => false,
    }
}

fn is_semi(tree: Option<&&TokenTree>) -> bool {
    match tree {
        Some(TokenTree::Token(token::Token {
            kind: token::TokenKind::Semi,
            ..
        })) => true,
        _ => false,
    }
}

type ParseResult<SuccessType> = Result<SuccessType, Option<Span>>;

fn parse_failure<SuccessType>(
    cx: &mut ExtCtxt,
    outer_span: Span,
    tt: Option<&TokenTree>,
) -> ParseResult<SuccessType> {
    match tt {
        Some(TokenTree::Token(t)) => {
            let s: Span = t.span;
            cx.span_err(s, &format!("Unexpected token: {:?}", t));
            Err(Some(s))
        }
        Some(TokenTree::Delimited(span, _, t)) => {
            let s: Span = span.entire();
            cx.span_err(s, &format!("Unexpected span: {:?}", t));
            Err(Some(s))
        }
        None => {
            cx.span_err(outer_span, "Unexpected end of input");
            Err(None)
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
        Some(TokenTree::Token(token)) => token.span,
        tt => return parse_failure(cx, outer_span, tt),
    };
    let rule_name = match iter.next() {
        Some(TokenTree::Token(token::Token {
            kind: token::Ident(t, _),
            ..
        })) => t,
        tt => return parse_failure(cx, s, tt),
    };
    match iter.next() {
        Some(TokenTree::Token(token::Token {
            kind: token::RArrow,
            ..
        })) => {}
        Some(TokenTree::Token(token)) => {
            let s: Span = token.span;
            cx.span_err(s, "Special rule name must be followed by ->");
            return Err(Some(s));
        }
        tt => return parse_failure(cx, s, tt),
    }

    let function_name = match iter.next() {
        Some(TokenTree::Token(token::Token {
            kind: token::Ident(tt, _),
            ..
        })) => tt.to_string(),
        tt => return parse_failure(cx, s, tt),
    };

    match iter.next() {
        Some(TokenTree::Token(token::Token {
            kind: token::Pound, ..
        })) => {}
        tt => return parse_failure(cx, s, tt),
    };

    let mut special_component = match iter.next() {
        Some(TokenTree::Token(token::Token {
            kind: token::Ident(tt, _),
            ..
        })) => {
            rsp = s;
            Terminal {
                identifier: tt.to_string(),
                full_path: tt.to_string(),
                span: s.to(rsp),
                conversion_fn: None,
            }
        }
        tt => return parse_failure(cx, s.to(rsp), tt),
    };

    while !is_semi(iter.peek()) {
        match iter.next() {
            Some(TokenTree::Token(token::Token {
                kind: token::ModSep,
                ..
            })) => {
                rsp = s;
                match iter.next() {
                    Some(TokenTree::Token(token::Token {
                        kind: token::Ident(tt, _),
                        ..
                    })) => {
                        rsp = s;
                        let right = tt.to_string();
                        special_component.full_path.push_str("::");
                        special_component.full_path.push_str(&right);
                        special_component.identifier = right;
                        special_component.span = special_component.span.to(s);
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

    Ok(special_component)
}

fn parse_enumdef(
    cx: &mut ExtCtxt,
    iter: &mut Peekable<Iter<'_, TokenTree>>,
    sp: Span,
) -> ParseResult<P<ast::Item>> {
    let mut res = Vec::new();
    let mut break_next = false;

    loop {
        match iter.next() {
            Some(t @ TokenTree::Delimited(..)) => {
                res.push(t.clone());
                if break_next {
                    break;
                }
                if let Some(
                    t @ TokenTree::Token(token::Token {
                        kind: token::Semi, ..
                    }),
                ) = iter.peek()
                {
                    res.push((*t).clone());
                    break;
                }
            }
            Some(
                t @ TokenTree::Token(token::Token {
                    kind: token::Ident(..),
                    ..
                }),
            ) => {
                res.push(t.clone());
                if let TokenTree::Token(token::Token {
                    kind: token::Ident(ident, _),
                    ..
                }) = t
                {
                    if ident.as_str() == "enum" || ident.as_str() == "struct" {
                        break_next = true;
                    }
                }
                if !break_next {
                    // enable constructs like @ type TypeName = Foo
                    if let Some(
                        t @ TokenTree::Token(token::Token {
                            kind: token::Semi, ..
                        }),
                    ) = iter.peek()
                    {
                        res.push((*t).clone());
                        break;
                    }
                }
            }
            Some(t) => res.push(t.clone()),
            None => {
                return parse_failure(
                    cx,
                    res.last()
                        .and_then(|t| {
                            if let TokenTree::Token(t) = t {
                                Some(t.span)
                            } else {
                                None
                            }
                        })
                        .unwrap_or(sp),
                    None,
                )
            }
        }
    }

    let sess = cx.parse_sess();
    let mut parser = parse::new_parser_from_tts(sess, res);

    Ok(parser.parse_item().unwrap().unwrap())
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
        Some(TokenTree::Token(token::Token {
            kind: token::Ident(t, _),
            span: s,
        })) => (s, t),
        tt => return parse_failure(cx, outer_span, tt),
    };
    match iter.next() {
        Some(TokenTree::Token(token::Token {
            kind: token::RArrow,
            ..
        })) => {}
        Some(TokenTree::Token(token)) => {
            let s: Span = token.span;
            cx.span_err(s, "Rule name must be followed by ->");
            return Err(Some(s));
        }
        tt => return parse_failure(cx, s.to(rsp), tt),
    }

    let mut components: Vec<Component> = Vec::new();
    let mut real_name: Option<String> = None;
    let mut current_components: Vec<RuleData> = Vec::new();
    let mut action = None;
    let mut priority: Option<(i32, bool)> = None;
    let mut enumdef = None;

    while !is_semi(iter.peek()) {
        match iter.next() {
            Some(TokenTree::Token(token::Token {
                kind:
                    token::TokenKind::Literal(token::Lit {
                        kind: token::LitKind::Integer,
                        symbol,
                        ..
                    }),
                ..
            })) => {
                let new_prio = symbol.to_string().parse().unwrap();
                priority = match iter.next() {
                    Some(TokenTree::Token(token::Token {
                        kind: token::TokenKind::Colon,
                        ..
                    })) => Some((new_prio, true)),
                    Some(TokenTree::Token(token::Token {
                        kind: token::Ident(tt, _),
                        ..
                    })) => match (iter.next(), tt.as_str().as_ref()) {
                        (
                            Some(TokenTree::Token(token::Token {
                                kind: token::Colon, ..
                            })),
                            "l",
                        ) => Some((new_prio.to_string().parse().unwrap(), true)),
                        (
                            Some(TokenTree::Token(token::Token {
                                kind: token::Colon, ..
                            })),
                            "r",
                        ) => Some((new_prio.to_string().parse().unwrap(), false)),
                        (tt, _) => return parse_failure(cx, s.to(rsp), tt),
                    },
                    tt => return parse_failure(cx, s.to(rsp), tt),
                };
            }
            Some(TokenTree::Token(token::Token {
                kind: token::Not, ..
            })) => {
                action = match iter.next() {
                    Some(TokenTree::Token(token::Token {
                        kind: token::Ident(tt, _),
                        ..
                    })) => Some(tt.to_string()),
                    tt => return parse_failure(cx, s.to(rsp), tt),
                };
            }
            Some(TokenTree::Token(token::Token {
                kind: token::Ident(tt, _),
                ..
            })) => {
                rsp = *s;
                current_components.push(RuleData {
                    identifier: tt.to_string(),
                    full_path: tt.to_string(),
                    span: rsp,
                    terminal,
                    indirect: indirect || tt.as_str() == t.as_str(),
                    conversion_fn: None,
                });
                terminal = false;
                indirect = false;
            }
            Some(TokenTree::Token(token::Token {
                kind: token::ModSep,
                span,
            })) => {
                rsp = *span;
                if let Some(mut data) = current_components.pop() {
                    match iter.next() {
                        Some(TokenTree::Token(token::Token {
                            kind: token::Ident(tt, _),
                            ..
                        })) => {
                            rsp = *s;
                            let right = tt.to_string();
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
            Some(TokenTree::Token(token::Token {
                kind: token::Pound,
                span,
            })) => {
                rsp = *span;
                terminal = true;
            }
            Some(TokenTree::Token(token::Token {
                kind: token::BinOp(token::BinOpToken::And),
                span,
            })) => {
                rsp = *span;
                indirect = true;
            }
            Some(TokenTree::Token(token::Token {
                kind: token::BinOp(token::BinOpToken::Or),
                span,
            })) => {
                rsp = *span;
                let real_real_name = real_name
                    .unwrap_or_else(|| current_components.get(0).unwrap().identifier.clone());
                real_name = None;
                components.push(Component {
                    real_name: real_real_name,
                    action,
                    rules: current_components,
                    priority,
                });
                action = None;
                priority = None;
                current_components = vec![];
            }
            Some(TokenTree::Token(token::Token {
                kind: token::Dot,
                span,
            })) => {
                rsp = *span;
                if let Some(data) = current_components.pop() {
                    real_name = Some(data.identifier);
                } else {
                    return parse_failure(cx, s.to(rsp), None);
                }
            }
            Some(TokenTree::Token(token::Token {
                kind: token::TokenKind::At,
                span,
            })) => {
                enumdef = Some(parse_enumdef(cx, iter, *span)?);
            }
            tt => return parse_failure(cx, s.to(rsp), tt),
        }
    }
    if !current_components.is_empty() {
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

    let identifier = t.as_str();
    let span = s.to(rsp);

    let rule = Rule {
        identifier: identifier.to_string(),
        span,
        data: components,
        enumdef,
    };

    if tm
        .push_rule(rule.identifier.clone(), rule.clone())
        .is_none()
    {
        cx.span_err(
            s.to(rsp),
            "Duplicate rule (Hint: Mark the expression A -> B; A -> C; with A -> B | C;)",
        );
        return Err(Some(s.to(rsp)));
    }

    Ok(rule)
}

fn expand_enums(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<dyn MacResult + 'static> {
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
        enumdef: None,
    });
    items.push(Rule {
        identifier: "$".to_string(),
        span: sp,
        data: vec![],
        enumdef: None,
    });
    for rule in &items {
        tm.push_rule(rule.identifier.clone(), rule.clone());
    }

    loop {
        match iter.peek() {
            Some(TokenTree::Token(token::Token {
                kind: token::Not, ..
            })) => match parse_special(cx, sp, &mut iter) {
                Ok(item) => {
                    terminals.insert(item);
                }
                Err(span) => return DummyResult::any(span.unwrap_or(sp)),
            },
            Some(_) => match parse_item(cx, sp, &mut iter, &mut tm) {
                Ok(item) => {
                    item.data.iter().for_each(|rules| {
                        rules.rules.iter().filter(|x| x.terminal).for_each(|x| {
                            terminals.insert(Terminal {
                                identifier: x.identifier.clone(),
                                full_path: x.full_path.clone(),
                                span: sp,
                                conversion_fn: None,
                            });
                        })
                    });

                    items.push(item);
                }
                Err(span) => return DummyResult::any(span.unwrap_or(sp)),
            },
            None => break,
        }
    }

    output_parser(cx, sp, &tm, &items, &terminals)
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("grammar", expand_enums);
}
