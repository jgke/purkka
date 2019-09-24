#![allow(dead_code)]

use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use proc_macro2::{Span, TokenStream};

#[derive(Debug, Clone)]
pub struct RuleData {
    pub identifier: String,
    pub full_path: String,
    pub span: Span,
    pub terminal: bool,
    pub indirect: bool,
    pub conversion_fn: Option<(String, String)>,
}

#[derive(Clone)]
pub struct Rule {
    pub identifier: String,
    pub span: Span,
    pub data: Vec<Component>,
    pub enumdef: Option<TokenStream>,
}

#[derive(Debug, Clone)]
pub struct Component {
    pub real_name: String,
    pub action: Option<String>,
    pub priority: Option<(i32, bool)>,
    pub rules: Vec<RuleData>,
}

#[derive(Debug, Clone)]
pub struct Terminal {
    pub identifier: String,
    pub full_path: String,
    pub span: Span,
    pub conversion_fn: Option<(String, String)>,
}

#[derive(Default)]
pub struct RuleTranslationMap {
    pub rules: HashMap<Index, Rule>,
    pub indices: HashMap<String, usize>,
    pub rev_indices: HashMap<usize, String>,

    pub current_index: usize,
}

// These actually arent safe at all to share between thread boundaries - but as long as the span
// fields arent't touched inside threaded parts, this is fine.
unsafe impl Send for RuleData {}
unsafe impl Send for Rule {}
unsafe impl Sync for RuleData {}
unsafe impl Sync for Rule {}

pub type Index = usize;

#[derive(Copy, PartialEq, Eq, Hash, Debug, Clone)]
pub struct Item {
    pub index: Index,
    pub subindex: usize,
    pub position: usize,
    pub lookahead: Index,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Core {
    pub index: Index,
    pub subindex: usize,
    pub position: usize,
}

impl Item {
    pub fn display<'a>(&'a self, tm: &'a RuleTranslationMap) -> ItemWithTr<'a> {
        ItemWithTr(tm, self)
    }

    pub fn to_core(self) -> Core {
        Core {
            index: self.index,
            subindex: self.subindex,
            position: self.position,
        }
    }
}

impl Core {
    pub fn display<'a>(&'a self, tm: &'a RuleTranslationMap) -> CoreWithTr<'a> {
        CoreWithTr(tm, self)
    }

    pub fn to_item(self, lookahead: Index) -> Item {
        Item {
            index: self.index,
            subindex: self.subindex,
            position: self.position,
            lookahead,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Action {
    Error,
    Shift(usize, Option<(i32, bool)>),
    Reduce(Index, usize, usize, Option<(i32, bool)>),
    Accept,
    Goto(usize),
}

#[derive(Clone, Debug)]
pub struct LRTable {
    pub actions: Vec<HashMap<Index, Action>>,
}

pub struct ItemWithTr<'a>(pub &'a RuleTranslationMap, pub &'a Item);
pub struct CoreWithTr<'a>(pub &'a RuleTranslationMap, pub &'a Core);

impl Ord for Item {
    fn cmp(&self, other: &Item) -> Ordering {
        self.index
            .cmp(&other.index)
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
        for Component {
            real_name, rules, ..
        } in &self.data
        {
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
        if self
            .indices
            .get(&name)
            .and_then(|index| self.rules.get(index))
            .is_some()
        {
            return None;
        }

        self.push_symbol(&name);
        rule.data.iter().for_each(|Component { rules, .. }| {
            rules
                .iter()
                .for_each(|ruledata| self.push_symbol(&ruledata.full_path))
        });

        self.rules.insert(self.indices[&name], rule);
        Some(())
    }

    fn push_symbol(&mut self, symbol: &str) {
        if self.indices.get(symbol).is_some() {
            return;
        }

        self.indices.insert(symbol.to_string(), self.current_index);
        self.rev_indices
            .insert(self.current_index, symbol.to_string());
        self.current_index += 1;
    }
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Action::Error => write!(f, "{: >7.5}", ""),
            Action::Shift(to, _priority) => {
                write!(f, "{: >7.5}", String::from("s") + &to.to_string())
            }
            Action::Reduce(rule, subrule, _length, _priority) => {
                write!(f, "{: >7.5}", format!("r{}{}", rule, subrule))
            }
            Action::Accept => write!(f, "{: >7.5}", "acc"),
            Action::Goto(to) => write!(f, "{: >7.5}", String::from("g") + &to.to_string()),
        }
    }
}

impl<'a> fmt::Display for ItemWithTr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        let ItemWithTr(tm, item) = self;
        let Component { ref rules, .. } = tm.rules[&item.index].data[item.subindex];
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

impl<'a> fmt::Display for CoreWithTr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::new();
        let CoreWithTr(tm, core) = self;
        let Component { ref rules, .. } = tm.rules[&core.index].data[core.subindex];
        for (i, symbol) in rules.iter().enumerate() {
            if i == core.position {
                output.push_str(" . ");
            } else {
                output.push_str(" ");
            }
            if symbol.terminal {
                output.push_str("#");
            }
            output.push_str(&symbol.identifier);
        }
        if core.position == rules.len() {
            output.push_str(" . ");
        }
        write!(f, "[{} ->{}]", core.index, output)
    }
}
