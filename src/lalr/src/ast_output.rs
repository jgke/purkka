use std::collections::{HashMap, HashSet};

use crate::types::{Component, Index, Rule, RuleTranslationMap, Terminal};
use syn::{Ident, Result};
use proc_macro2::TokenStream;
use quote::quote;

pub fn first(
    tm: &RuleTranslationMap,
    set: &mut HashSet<Index>,
    cache: &mut HashSet<Index>,
    rule_index: Index,
) -> bool {
    if cache.get(&rule_index).is_some() {
        return false;
    }
    cache.insert(rule_index);
    let rule = &tm.rules.get(&rule_index).unwrap_or_else(|| {
        panic!(
            "No rule found for index {} ({:?})",
            rule_index,
            tm.rev_indices.get(&rule_index)
        )
    });
    let mut has_e = false;
    // E -> A | B
    for (i, _) in rule.data.iter().enumerate() {
        let mut sub_cache = cache.clone();
        has_e |= first_loop(tm, set, &mut sub_cache, (rule_index, i, 0));
    }
    has_e
}

fn first_loop(
    tm: &RuleTranslationMap,
    set: &mut HashSet<Index>,
    cache: &mut HashSet<Index>,
    rule: (Index, usize, usize),
) -> bool {
    let (rule_index, rule_subindex, position) = rule;
    let rule = &tm.rules[&rule_index];
    let Component { rules, .. } = &rule.data[rule_subindex];
    let mut has_e = true;
    // E -> A
    for (i, ruledata) in rules.iter().enumerate() {
        if i < position {
            continue;
        }
        // A
        if ruledata.identifier == "Epsilon" {
            has_e = true;
        } else if ruledata.terminal {
            set.insert(tm.indices[&ruledata.full_path]);
            has_e = false;
            break;
        } else {
            let sub_has_e = first(tm, set, cache, tm.indices[&ruledata.full_path]);
            if !sub_has_e {
                has_e = false;
                break;
            }
        }
    }
    has_e
}

struct AstBuilderCx<'a> {
    tm: &'a RuleTranslationMap,
    special_rules: &'a HashMap<String, String>,
}

impl<'a> AstBuilderCx<'a> {
    fn get_enum_item(&self, rule: &Rule) -> TokenStream {
        let enum_name = &rule.identifier;
        let enum_ident = Ident::new(&rule.identifier, rule.span);
        let variants: TokenStream = rule
            .data
            .clone()
            .into_iter()
            .map(
                |Component {
                     real_name, rules, ..
                 }| {
                    let vals: TokenStream = rules
                        .clone()
                        .into_iter()
                        .filter(|r| r.identifier != "Epsilon")
                        .map(|item| {
                            let ident_arr = item.full_path.rsplitn(2, "::");
                            let ident_str = ident_arr.last().unwrap();
                            let ident = Ident::new(ident_str, item.span);
                            match self.special_rules.get(ident_str) {
                                Some(_) => (quote! {#ident,}),
                                None => {
                                    if item.indirect || &item.identifier == enum_name {
                                        (quote! {Box<#ident>,})
                                    } else {
                                        (quote! {#ident,})
                                    }
                                }
                            }
                        })
                        .collect();
                    let real_ident = Ident::new(&real_name, rules[0].span);
                    quote! {
                        #real_ident(#vals),
                    }
                },
            )
            .collect();

        quote! {
            #[derive(Clone, Debug, PartialEq)]
            pub enum #enum_ident {
                #variants
            }
        }
    }

    pub fn get_first_fn(&self, rules: &[Rule]) -> TokenStream {
        let header = r#"
    #[macro_export]
    macro_rules! match_first {
        ($val:expr => $bind:ident, default $else:expr, $($body:tt)* ) => {
            match_first!(@rules $val, $bind, $else, {}, $($body)* )
        };
        "#;

        let single_match_start = r#"(@rules $val:expr, $bind:ident, $else:expr, {$($arms:tt)*}, "#;
        let single_match_middle = r#" => $e:expr, $($tail:tt)*) => {
            match_first!(@rules $val, $bind, $else, {
                $($arms)* "#;
        let single_match_tail = r#"
            },
            $($tail)*)
        };
        "#;

        let tail = r#"
        (@rules $val:expr, $bind:ident, $else:expr, {$($arms:tt)*}, $(,)*) => {
            match $val {
                $($arms)*
                $bind => { $else }
            }
        };
   }
        "#;

        let mut result = header.to_string();
        for rule in rules.iter().filter(|t| t.identifier != "$") {
            let mut single_match = single_match_start.to_string();
            single_match += &rule.identifier;
            single_match += single_match_middle;
            let mut first_terms = HashSet::new();
            let _has_e = first(
                self.tm,
                &mut first_terms,
                &mut HashSet::new(),
                self.tm.indices[&rule.identifier],
            );
            single_match += &first_terms
                .iter()
                .map(|t| format!("Some($bind @ {}(..))", self.tm.rev_indices[t]))
                .collect::<Vec<String>>()
                .join("|");
            single_match += " => { $e }";
            single_match += single_match_tail;
            result += &single_match;
        }
        result += tail;

        result.parse().unwrap()
    }
}

pub fn output_parser(
    tm: &RuleTranslationMap,
    rules: &[Rule],
    terminals: &HashSet<Terminal>,
) -> Result<TokenStream> {
    let special_rules: HashMap<String, String> = terminals
        .iter()
        .filter(|term| term.conversion_fn.is_some())
        .map(|term| {
            let identifier = term.identifier.clone();
            let dependant = term.conversion_fn.as_ref().unwrap().1.clone();
            (identifier, dependant)
        })
        .collect();

    let mut special_provides: HashMap<String, Vec<(String, String)>> = HashMap::new();

    for term in terminals {
        if term.conversion_fn.is_none() {
            continue;
        }

        let identifier = term.identifier.clone();
        let dependant = term.conversion_fn.as_ref().unwrap().1.clone();
        let provider = term.conversion_fn.as_ref().unwrap().0.clone();
        let list = special_provides.entry(dependant).or_insert_with(Vec::new);
        list.push((identifier, provider));
    }

    let builder = AstBuilderCx {
        tm,
        special_rules: &special_rules,
    };

    let items: TokenStream = rules
        .iter()
        .filter(|item| item.identifier != "Epsilon")
        .filter(|item| item.identifier != "$")
        .map(|item|
            item.enumdef
                .clone()
                .unwrap_or_else(|| builder.get_enum_item(item)))
        .collect();

    let all_structs_enum: TokenStream = rules
        .iter()
        .filter(|item| item.identifier != "$")
        .map(|rule| {
            let ident = Ident::new(&rule.identifier, rule.span);
            (quote! { #ident(#ident), })
        })
        .chain(terminals.iter().map(|term| {
            let ident = Ident::new(&term.identifier, term.span);
            let var = quote! { #ident(Token), };
            var
        }))
        .collect();

    let epsilon = quote! {
        #[derive(Clone, Debug, PartialEq)]
        pub enum Epsilon {
            Epsilon()
        }
    };

    let all_data = quote! {
        #[derive(Clone, Debug, PartialEq)]
        pub enum _Data {
            #all_structs_enum
        }
    };

    let first_fn = builder.get_first_fn(rules);

    let res = items.into_iter().chain(epsilon).chain(all_data).chain(first_fn).collect();

    //println!("{}", res);

    Ok(res)
}
