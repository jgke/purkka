extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use syn::{parse_macro_input, Token, Item, Result, Ident, LitInt, spanned::Spanned};
use syn::parse::{Parse, ParseStream};
use quote::ToTokens;

use std::collections::HashSet;

mod ast_output;
mod types;

use ast_output::output_parser;
use types::{Component, Rule, RuleData, RuleTranslationMap, Terminal};

fn expand_enums(iter: ParseStream) -> Result<TokenStream> {
    let mut tm = RuleTranslationMap {
        ..Default::default()
    };
    let mut terminals: HashSet<Terminal> = HashSet::new();
    let sp = Span::call_site();

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
        if iter.peek(Token![!]) {
            terminals.insert(parse_special(sp, iter));
        } else if !iter.is_empty() {
            let item = parse_item(sp, iter, &mut tm)?;
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
        } else {
            break
        }
    }

    output_parser(sp, &tm, &items, &terminals)
}

fn parse_special(
    outer_span: Span,
    iter: ParseStream,
) -> Terminal {
    panic!("69")
}

/*
fn parse_special(
    outer_span: Span,
    iter: ParseStream,
) -> Terminal {
    let mut rsp = outer_span;
    let s = match iter.next() {
        Some(token) => token.span,
        tt => return parse_failure(outer_span, tt),
    };
    let rule_name = match iter.next() {
        Some(token::Token {
            kind: token::Ident(t, _),
            ..
        }) => t,
        tt => return parse_failure(s, tt),
    };
    match iter.next() {
        Some(token::Token {
            kind: token::RArrow,
            ..
        }) => {}
        Some(token) => {
            panic!()
            //let s: Span = token.span;
            //cx.span_err(s, "Special rule name must be followed by ->");
            //return Err(Some(s));
        }
        tt => return parse_failure(cx, s, tt),
    }

    let function_name = match iter.next() {
        Some(token::Token {
            kind: token::Ident(tt, _),
            ..
        }) => tt.to_string(),
        tt => return parse_failure(cx, s, tt),
    };

    match iter.next() {
        Some(token::Token {
            kind: token::Pound, ..
        }) => {}
        tt => return parse_failure(cx, s, tt),
    };

    let mut special_component = match iter.next() {
        Some(token::Token {
            kind: token::Ident(tt, _),
            ..
        }) => {
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
            Some(token::Token {
                kind: token::ModSep,
                ..
            }) => {
                rsp = s;
                match iter.next() {
                    Some(token::Token {
                        kind: token::Ident(tt, _),
                        ..
                    }) => {
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
*/

fn parse_enumdef(
    iter: ParseStream,
    sp: Span,
) -> Item {
    panic!("174")
}

fn parse_item(
    outer_span: Span,
    iter: ParseStream,
    tm: &mut RuleTranslationMap) -> Result<Rule> {

    let mut rsp = outer_span;
    let mut terminal = false;
    let mut indirect = false;
    let t: Ident = iter.parse()?;
    let s = t.span();
    iter.parse::<Token![->]>()?;
    let mut components: Vec<Component> = Vec::new();
    let mut real_name: Option<String> = None;
    let mut current_components: Vec<RuleData> = Vec::new();
    let mut action = None;
    let mut priority: Option<(i32, bool)> = None;
    let mut enumdef = None;

    while !iter.peek(Token![;]) {
        if iter.peek(LitInt) {
            let new_prio: LitInt = iter.parse()?;
            priority = if iter.peek(Token![:]) {
                iter.parse::<Token![:]>()?;
                Some((new_prio.base10_parse()?, true))
            } else {
                let ident: Ident = iter.parse()?;
                iter.parse::<Token![:]>()?;
                match ident.to_string().as_ref() {
                    "l" => Some((new_prio.to_string().parse().unwrap(), true)),
                    "r" => Some((new_prio.to_string().parse().unwrap(), false)),
                    _ => panic!("Not l/r")
                }
            };
        } else if iter.peek(Token![!]) {
            action = Some(iter.parse::<Ident>()?.to_string());
        } else if iter.peek(Ident) {
            let tt = iter.parse::<Ident>()?;
            rsp = tt.span();
            current_components.push(RuleData {
                identifier: tt.to_string(),
                full_path: tt.to_string(),
                span: rsp,
                terminal,
                indirect: indirect || tt.to_string() == t.to_string(),
                conversion_fn: None,
            });
            terminal = false;
            indirect = false;
        } else if iter.peek(Token![::]) {
            rsp = iter.parse::<Token![::]>()?.span();
            if let Some(mut data) = current_components.pop() {
                let tt = iter.parse::<Ident>()?;
                rsp = tt.span();
                let right = tt.to_string();
                data.full_path.push_str("::");
                data.full_path.push_str(&right);
                data.identifier = right;
                data.span = data.span.join(s).unwrap_or(data.span);
                current_components.push(data);
            } else {
                panic!("307")
            }
        } else if iter.peek(Token![#]) {
            rsp = iter.parse::<Token![#]>()?.span;
            terminal = true;
        } else if iter.peek(Token![&]) {
            rsp = iter.parse::<Token![&]>()?.span;
            indirect = true;
        } else if iter.peek(Token![|]) {
            rsp = iter.parse::<Token![|]>()?.span;
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
        } else if iter.peek(Token![.]) {
            rsp = iter.parse::<Token![.]>()?.span;
            if let Some(data) = current_components.pop() {
                real_name = Some(data.identifier);
            } else {
                panic!("334")
            }
        } else if iter.peek(Token![@]) {
            iter.parse::<Token![@]>()?;
            enumdef = Some(iter.parse::<Item>()?.into_token_stream());
            break;
        } else {
            panic!("340")
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

    if iter.peek(Token![;])  {
        iter.parse::<Token![;]>()?;
    }

    let identifier = t.to_string();
    let span = s.join(rsp).unwrap_or(s);

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
        panic!(370)
        //cx.span_err(
        //    s.to(rsp),
        //    "Duplicate rule (Hint: Mark the expression A -> B; A -> C; with A -> B | C;)",
        //);
    }

    Ok(rule)
}

struct Foo {
    result: TokenStream
}

impl Parse for Foo {
    fn parse(buf: ParseStream) -> Result<Self> {
        let result = expand_enums(buf)?;
        Ok(Foo { result })
    }
}

#[proc_macro]
pub fn grammar(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from((parse_macro_input!(input as Foo)).result)
}
