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

    while !iter.is_empty() {
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
    }

    output_parser(&tm, &items, &terminals)
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
            iter.parse::<Token![::]>()?.span();
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
