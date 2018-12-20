use syntax::ast;
use smallvec::SmallVec;
use syntax::ext::base::{MacEager, MacResult};
use syntax::ptr::P;
use syntax::ext::quote::rt::Span;

use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

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
    pub data: Vec<Vec<RuleData>>
}

#[derive(Default)]
pub struct RuleTranslationMap {
    pub rule_to_number: HashMap<Rc<String>, i64>,
    pub number_to_rule: HashMap<i64, Rc<String>>,
    pub rules: HashMap<i64, Vec<Rule>>,
    pub rule_number: i64
}

impl RuleTranslationMap {
    pub fn push_rule(&mut self, rule: String, data: Rule) -> Option<i64> {
        if self.rule_to_number.get(&rule).is_some() {
            return None;
        }

        let num = self.rule_number;
        let r = Rc::new(rule);
        self.rule_number += 1;
        self.rule_to_number.insert(Rc::clone(&r), num);
        self.number_to_rule.insert(num, r);
        self.rules.insert(num, vec!(data));
        return Some(num);
    }

    pub fn add_to_rule(&mut self, rule: i64, data: Rule) {
        match self.rules.get_mut(&rule) {
            Some(rules) => rules.push(data),
            None => panic!("BUG: tried to add to nonexistant rule")
        }
    }
}

type Item = (i64, i64);

//pub fn closure(items: &mut HashSet<Item>) {
//    let mut added: HashSet<Item> = items.clone();
//    let mut added_next: HashSet<Item> = HashSet::new();
//
//    while added.len() > 0 {
//        added_next.clear();
//
//        added.iter().for_each(|item| {
//            added.insert(item.clone());
//        })
//    }
//}

pub fn compute_lalr(tm: RuleTranslationMap, parser_items: Vec<Rule>, items: SmallVec<[P<ast::Item>; 1]>)
        -> Box<(dyn MacResult + 'static)> {
    println!("{:?}", tm.rule_to_number);
    println!("{:?}", tm.number_to_rule);
    println!("{:?}", parser_items);
    return MacEager::items(items)
}
