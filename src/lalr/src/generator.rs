use std::fmt;
use syntax::ast;
use smallvec::SmallVec;
use syntax::ext::base::{MacEager, MacResult};
use syntax::ptr::P;
use syntax::ext::quote::rt::Span;

use std::collections::HashMap;
use std::collections::HashSet;

static EPSILON: &str = "Epsilon";

#[derive(Debug, Clone)]
pub struct RuleData {
    pub identifier: String,
    pub full_path: String,
    pub span: Span,
    pub terminal: bool,
    pub indirect: bool
}

#[derive(Debug, Clone)]
pub struct Rule {
    pub identifier: String,
    pub span: Span,
    pub data: Vec<(String, Vec<RuleData>)>
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        let indentation = " ".repeat(self.identifier.len() + 2);
        let mut first = true;
        for (name, rules) in &self.data {
            if !first {
                output.push_str("\n");
                output.push_str(&indentation);
                output.push_str("| ");
            }
            first = false;
            output.push_str(&name);
            output.push_str(". ");
            for ruledata in rules {
                if ruledata.indirect {
                    output.push_str("&");
                }
                if ruledata.terminal {
                    output.push_str("#");
                }
                output.push_str(&ruledata.identifier);
                output.push_str(" ");
            }
        }
        write!(f, "{} -> {}", self.identifier, output)
    }
}

#[derive(Default)]
pub struct RuleTranslationMap {
    pub rule_to_number: HashMap<String, usize>,
    pub number_to_rule: HashMap<usize, String>,
    pub rules: Vec<Rule>,
    pub rule_number: usize
}

impl RuleTranslationMap {
    pub fn push_rule(&mut self, rule: String, data: Rule) -> Option<usize> {
        if self.rule_to_number.get(&rule).is_some() {
            return None;
        }

        let num = self.rule_number;
        self.rule_number += 1;
        self.rule_to_number.insert(rule.clone(), num);
        self.number_to_rule.insert(num, rule.clone());
        self.rules.push(data);
        return Some(num);
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Item {
    pub rule: (i64, usize),
    pub position: usize,
    pub lookahead: String
}

fn first(tm: &RuleTranslationMap, set: &mut HashSet<String>, cache: &mut HashSet<usize>, rule_index: usize)
    -> bool {
    if cache.get(&rule_index).is_some() {
        return false;
    }
    cache.insert(rule_index);
    let rule = &tm.rules[rule_index];
    let mut has_e = false;
    // E -> A | B
    for (_, symbols) in &rule.data {
        let mut any_has_e = true;
        // E -> A
        for ref ruledata in symbols {
            // A
            if ruledata.identifier == "Epsilon" {
                has_e = true;
            } else if ruledata.terminal {
                set.insert(ruledata.full_path.clone());
                any_has_e = false;
                break;
            } else {
                let sub_has_e = first(tm, set, cache, tm.rule_to_number[&ruledata.identifier]);
                if !sub_has_e {
                    any_has_e = false;
                    break;
                }
            }
        }
        has_e = has_e || any_has_e;
    }
    has_e
}

/*
pub fn closure(tm: &RuleTranslationMap, items: &mut HashSet<&Item>) {
    let mut added: HashSet<&Item> = HashSet::new();
    let mut prevsize = items.len() + 1;
    for item in items.iter() {
        added.insert(item);
    }

    while prevsize > items.len() {
        prevsize = items.len();
        for item in &added {
            let (rule_index, subrule_index) = item.rule;
            for (_, rule) in &tm.rules[rule_index].data {
                if rule[item.position].terminal {
                } else {
                }
            }
        }

        for ref item in &added {
            items.insert(item);
        }

        added.clear();
    }
}
*/

pub fn compute_lalr(tm: RuleTranslationMap, parser_items: Vec<Rule>, items: SmallVec<[P<ast::Item>; 1]>)
        -> Box<(dyn MacResult + 'static)> {
    println!("{:?}", tm.rule_to_number);
    println!("{:?}", tm.number_to_rule);
    for item in parser_items {
        println!("{}", item);
    }
    let mut set = HashSet::new();
    let mut cache = HashSet::new();
    first(&tm, &mut set, &mut cache, 0);
    println!("{:?}", set);
    return MacEager::items(items)
}
