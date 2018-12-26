#![allow(dead_code)]

use syntax::ext::quote::rt::Span;

use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;

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
    pub rules: HashMap<String, Rule>,
    pub indices: HashMap<String, usize>,

    pub current_index: usize
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Item {
    pub index: String,
    pub subindex: usize,
    pub position: usize,
    pub lookahead: String
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Action {
    Error,
    Shift(usize),
    Reduce(String, usize),
    Accept,
    Goto(usize)
}

#[derive(Debug)]
pub struct LRTable {
    pub actions: Vec<HashMap<String, Action>>
}

pub struct ItemWithTr<'a>(pub &'a RuleTranslationMap, pub &'a Item);

impl Ord for Item {
    fn cmp(&self, other: &Item) -> Ordering {
        rule_name_compare(&self.index, &other.index)
            .then(self.subindex.cmp(&other.subindex))
            .then(self.position.cmp(&other.position))
            .then(self.lookahead.cmp(&other.lookahead))
    }
}

impl PartialOrd for Item {
    fn partial_cmp(&self, other: &Item) -> Option<Ordering> {
        Some(self.cmp(other))
    }
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
    pub fn push_rule(&mut self, name: String, rule: Rule) -> Option<()> {
        if self.rules.get(&name).is_some() {
            return None;
        }

        rule.data.iter().for_each(
            |(_, rules)| rules.iter().for_each(
                |ruledata| self.push_symbol(&ruledata.full_path)));

        self.rules.insert(name, rule);
        return Some(());
    }

    fn push_symbol(&mut self, symbol: &str) {
        if self.indices.get(symbol).is_some() {
            return;
        }

        self.current_index += 1;
        self.indices.insert(symbol.to_string(), self.current_index);
    }
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Action::Error => write!(f, "{: >5}", ""),
            Action::Shift(to) => write!(f, "{: >5}", String::from("s") + &to.to_string()),
            Action::Reduce(rule, subrule) => write!(f, "{: >5}", String::from("r") + rule + &subrule.to_string()),
            Action::Accept => write!(f, "{: >5}", "acc"),
            Action::Goto(to) => write!(f, "{: >5}", to),
        }
    }
}

impl<'a> fmt::Display for ItemWithTr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        let ItemWithTr(tm, item) = self;
        let (ref _name, ref symbols) = tm.rules[&item.index].data[item.subindex];
        for (i, symbol) in symbols.iter().enumerate() {
            if i == item.position {
                output.push_str(" . ");
            } else {
                output.push_str(" ");
            }
            if symbol.terminal {
                output.push_str("#");
            }
            output.push_str(&symbol.identifier);
        }
        if item.position == symbols.len() {
                output.push_str(" . ");
        }
        write!(f, "[{} ->{}, {}]", item.index, output, item.lookahead)
    }
}

pub fn rule_name_compare(this: &String, that: &String) -> Ordering {
    if this == "S" {
        Ordering::Less
    } else if that == "S" {
        Ordering::Greater
    } else if this == "SS" {
        Ordering::Less
    } else if that == "SS" {
        Ordering::Greater
    } else {
        this.cmp(that)
    }
}
