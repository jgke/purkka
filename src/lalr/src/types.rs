#![allow(dead_code)]

use syntax_pos::Span;

use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub struct RuleData {
    pub identifier: String,
    pub full_path: String,
    pub span: Span,
    pub terminal: bool,
    pub indirect: bool,
    pub conversion_fn: Option<(String, String)>,
}

#[derive(Debug, Clone)]
pub struct Rule {
    pub identifier: String,
    pub span: Span,
    pub data: Vec<Component>,
}

#[derive(Debug, Clone)]
pub struct Component {
    pub real_name: String,
    pub action: Option<String>,
    pub rules: Vec<RuleData>
}

#[derive(Debug, Clone)]
pub struct Terminal {
    pub identifier: String,
    pub full_path: String,
    pub span: Span,
    pub conversion_fn: Option<(String, String)>,
}

#[derive(Debug, Default)]
pub struct RuleTranslationMap {
    pub rules: HashMap<String, Rule>,
    pub indices: HashMap<String, usize>,

    pub current_index: usize,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Item {
    pub index: String,
    pub subindex: usize,
    pub position: usize,
    pub lookahead: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Action {
    Error,
    Shift(usize),
    Reduce(String, usize, usize),
    Accept,
    Goto(usize),
}

#[derive(Debug)]
pub struct LRTable {
    pub actions: Vec<HashMap<String, Action>>,
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
        for Component {real_name, rules, ..} in &self.data {
            if !first {
                output.push_str("\n");
                output.push_str(&indentation);
                output.push_str("| ");
            }
            first = false;
            output.push_str(&real_name);
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

impl Hash for Terminal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.identifier.hash(state);
        self.full_path.hash(state);
    }
}

impl PartialEq for Terminal {
    fn eq(&self, other: &Terminal) -> bool {
        self.identifier == other.identifier && self.full_path == other.full_path
    }
}
impl Eq for Terminal {}

impl RuleTranslationMap {
    pub fn push_rule(&mut self, name: String, rule: Rule) -> Option<()> {
        if self.rules.get(&name).is_some() {
            return None;
        }

        self.push_symbol(&name);
        rule.data.iter().for_each(|Component {rules, ..}| {
            rules
                .iter()
                .for_each(|ruledata| self.push_symbol(&ruledata.full_path))
        });

        self.rules.insert(name, rule);
        return Some(());
    }

    fn push_symbol(&mut self, symbol: &str) {
        if self.indices.get(symbol).is_some() {
            return;
        }

        self.indices.insert(symbol.to_string(), self.current_index);
        self.current_index += 1;
    }
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Action::Error => write!(f, "{: >7.5}", ""),
            Action::Shift(to) => write!(f, "{: >7.5}", String::from("s") + &to.to_string()),
            Action::Reduce(rule, subrule, _length) => write!(
                f,
                "{: >7.5}",
                String::from("r") + rule + &subrule.to_string()
            ),
            Action::Accept => write!(f, "{: >7.5}", "acc"),
            Action::Goto(to) => write!(f, "{: >7.5}", String::from("g") + &to.to_string()),
        }
    }
}

impl<'a> fmt::Display for ItemWithTr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        let ItemWithTr(tm, item) = self;
        let Component {ref real_name, ref rules, ..} = tm.rules[&item.index].data[item.subindex];
        for (i, symbol) in rules.iter().enumerate() {
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
        if item.position == rules.len() {
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
