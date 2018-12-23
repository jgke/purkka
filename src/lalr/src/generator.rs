use std::fmt;
use syntax::ast;
use smallvec::SmallVec;
use syntax::ext::base::{MacEager, MacResult};
use syntax::ptr::P;
use syntax::ext::quote::rt::Span;

use std::collections::HashMap;
use std::collections::HashSet;

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

#[derive(Default)]
pub struct RuleTranslationMap {
    pub rule_to_number: HashMap<String, usize>,
    pub number_to_rule: HashMap<usize, String>,
    pub rules: Vec<Rule>,
    pub rule_number: usize
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Item {
    pub rule: (usize, usize),
    pub position: usize,
    pub lookahead: String
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

struct ItemWithTr<'a>(pub &'a RuleTranslationMap, pub &'a Item);

impl<'a> fmt::Display for ItemWithTr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        let ItemWithTr(tm, item) = self;
        let (rule_index, rule_subindex) = item.rule;
        let (ref name, ref symbols) = tm.rules[rule_index].data[rule_subindex];
        for (i, symbol) in symbols.iter().enumerate() {
            if i == item.position {
                output.push_str(" . ");
            }
            if symbol.terminal {
                output.push_str("#");
            }
            output.push_str(&symbol.identifier);
        }
        write!(f, "[{} -> {}({}), {}]", tm.number_to_rule[&rule_index], name, output, item.lookahead)
    }
}

fn first(tm: &RuleTranslationMap, set: &mut HashSet<String>, rule_index: usize) -> bool {
    let mut cache = HashSet::new();
    _first(tm, set, &mut cache, rule_index)
}

fn _first(tm: &RuleTranslationMap, set: &mut HashSet<String>, cache: &mut HashSet<usize>, rule_index: usize) -> bool {
    let rule = &tm.rules[rule_index];
    let mut has_e = false;
    // E -> A | B
    for (i, _) in rule.data.iter().enumerate() {
        has_e |= _first_all(tm, set, cache, (rule_index, i, 0));
    }
    has_e
}

fn _first_all(tm: &RuleTranslationMap, set: &mut HashSet<String>, cache: &mut HashSet<usize>,
          rule: (usize, usize, usize)) -> bool {
    let (rule_index, rule_subindex, position) = rule;
    if cache.get(&rule_index).is_some() {
        return false;
    }
    cache.insert(rule_index);
    let rule = &tm.rules[rule_index];
    let (_, symbols) = &rule.data[rule_subindex];
    let mut has_e = true;
        // E -> A
    for (i, ruledata) in symbols.iter().enumerate() {
        if i < position {
            continue;
        }
        // A
        if ruledata.identifier == "Epsilon" {
            has_e = true;
            break;
        } else if ruledata.terminal {
            set.insert(ruledata.full_path.clone());
            has_e = false;
            break;
        } else {
            let sub_has_e = _first(tm, set, cache, tm.rule_to_number[&ruledata.identifier]);
            if !sub_has_e {
                has_e = false;
                break;
            }
        }
    }
    has_e
}

pub fn closure(tm: &RuleTranslationMap, items: &mut HashSet<Item>) {
    let mut added: HashSet<Item> = HashSet::new();
    let mut added_next: HashSet<Item> = HashSet::new();
    let mut prevsize = items.len() + 1;
    for item in items.iter() {
        added.insert(item.clone());
    }

    while prevsize > items.len() {
        prevsize = items.len();
        // for each item [A -> a.Bb, a] in I
        for item in &added {
            let (rule_index, _) = item.rule;
            // for each production [B -> g] in G
            for (i, (_, rule)) in tm.rules[rule_index].data.iter().enumerate() {
                if rule[item.position].terminal {
                    // ???
                } else {
                    let mut set = HashSet::new();
                    let production_number  = tm.rule_to_number[&rule[item.position].identifier];
                    if first(tm, &mut set, production_number) {
                        //added.insert(Item {
                        //    rule: (rule_index, i),
                        //    position: 0,
                        //    lookahead: item.
                        //});
                    }
                    // for each terminal b in First(Ba)
                    for terminal in set {
                        added_next.insert(Item {
                            rule: (rule_index, i),
                            position: 0,
                            lookahead: terminal
                        });
                    }
                }
            }
        }

        for item in &added_next {
            items.insert(item.clone());
        }

        added = added_next;
        added_next = HashSet::new();
    }
}

pub fn compute_lalr(tm: RuleTranslationMap, parser_items: Vec<Rule>, output: SmallVec<[P<ast::Item>; 1]>)
        -> Box<(dyn MacResult + 'static)> {
    println!("{:?}", tm.rule_to_number);
    println!("{:?}", tm.number_to_rule);
    for item in parser_items {
        println!("{}", item);
    }

    let mut set = HashSet::new();
    first(&tm, &mut set, 0);
    println!("{:?}", set);

    let mut items = HashSet::new();
    items.insert(Item {
        rule: (0, 0),
        position: 0,
        lookahead: "$".to_string()
    });
    closure(&tm, &mut items);
    for item in items {
        println!("{}", ItemWithTr(&tm, &item));
    }

    return MacEager::items(output)
}

#[cfg(test)]
mod tests {
    #[test]
    fn first() {

    }
}
