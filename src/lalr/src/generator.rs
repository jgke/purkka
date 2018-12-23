use std::fmt;
use syntax::ast;
use smallvec::SmallVec;
use syntax::ext::base::{MacEager, MacResult};
use syntax::ptr::P;
use syntax::ext::quote::rt::Span;

use std::cmp::Ordering;
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
    pub rules: HashMap<String, Rule>
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Item {
    pub index: String,
    pub subindex: usize,
    pub position: usize,
    pub lookahead: String
}

#[derive(Debug, PartialEq, Eq)]
pub enum Action {
    Error,
    Shift(usize),
    Reduce(String, usize),
    Accept,
    Goto(usize)
}

#[derive(Debug)]
pub struct LRTable {
    actions: Vec<HashMap<String, Action>>
}

fn rule_name_compare(this: &String, that: &String) -> Ordering {
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
    pub fn push_rule(&mut self, rule: String, data: Rule) -> Option<()> {
        if self.rules.get(&rule).is_some() {
            return None;
        }

        self.rules.insert(rule, data);
        return Some(());
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

struct ItemWithTr<'a>(pub &'a RuleTranslationMap, pub &'a Item);

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

fn first(tm: &RuleTranslationMap, set: &mut HashSet<String>, rule_index: &str) -> bool {
    let mut cache = HashSet::new();
    _first(tm, set, &mut cache, rule_index)
}

fn _first(tm: &RuleTranslationMap, set: &mut HashSet<String>, cache: &mut HashSet<String>, rule_index: &str) -> bool {
    if cache.get(rule_index).is_some() {
        return false;
    }
    cache.insert(rule_index.to_string());
    let rule = &tm.rules[rule_index];
    let mut has_e = false;
    // E -> A | B
    for (i, _) in rule.data.iter().enumerate() {
        has_e |= _first_all(tm, set, cache, (&rule_index, i, 0));
    }
    has_e
}

fn _first_all(tm: &RuleTranslationMap, set: &mut HashSet<String>, cache: &mut HashSet<String>,
          rule: (&str, usize, usize)) -> bool {
    let (rule_index, rule_subindex, position) = rule;
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
            let sub_has_e = _first(tm, set, cache, ruledata.identifier.as_str());
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
    let mut prevsize = items.len();
    for item in items.iter() {
        added.insert(item.clone());
    }

    while prevsize < items.len() + 1 {
        prevsize = items.len() + 1;
        // for each item [A -> a.Bb, a] in I
        for item in &added {
            let (_, production_rules) = &tm.rules[&item.index].data[item.subindex];
            match &production_rules.get(item.position) {
                None => {},
                Some(RuleData { terminal: true, .. }) => {},
                Some(current_production) => {
                let inner_production = &tm.rules[&current_production.identifier];
                // for each production [B -> g] in G
                for (i, _) in inner_production.data.iter().enumerate() {
                    let mut set = HashSet::new();
                    let mut cache = HashSet::new();
                    // for each terminal b in First(Ba)
                    if _first_all(tm, &mut set, &mut cache,
                                  (&item.index, item.subindex, item.position + 1)) {
                        added_next.insert(Item {
                            index: inner_production.identifier.clone(),
                            subindex: i,
                            position: 0,
                            lookahead: item.lookahead.to_string()
                        });
                    }
                    for terminal in set {
                        added_next.insert(Item {
                            index: inner_production.identifier.clone(),
                            subindex: i,
                            position: 0,
                            lookahead: terminal
                        });
                    }
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

pub fn goto(tm: &RuleTranslationMap, goto_items: &mut HashSet<Item>, items: &HashSet<Item>, rule: String) {
    for item in items {
        let (_, ruledata) = &tm.rules[&item.index].data[item.subindex];
        if ruledata.len() > item.position && ruledata[item.position].full_path == rule {
            let mut goto_item = item.clone();
            goto_item.position += 1;
            goto_items.insert(goto_item);
        }
    }
    closure(tm, goto_items);
}

pub fn items(tm: &RuleTranslationMap, items: &mut Vec<HashSet<Item>>, symbols: &Vec<String>) {
    let mut initial = HashSet::new();
    println!("{:?}", symbols);
    initial.insert(Item {
        index: "S".to_string(),
        subindex: 0,
        position: 0,
        lookahead: "$".to_string()
    });
    closure(&tm, &mut initial);
    items.push(initial.clone());
    let mut added = Vec::new();
    let mut added_next: Vec<HashSet<Item>> = Vec::new();
    added.push(initial);
    let mut prevsize = 0;

    while prevsize < items.len() {
        prevsize = items.len();
        for set in &added {
            println!("{:?}", set);
            for symbol in symbols {
                let mut goto_items = HashSet::new();
                goto(tm, &mut goto_items, set, symbol.to_string());
                println!("{:?}", goto_items);
                if goto_items.len() > 0 && !items.contains(&goto_items) && !added_next.contains(&goto_items) {
                    added_next.push(goto_items);
                }
            }
        }
        for item in &added_next {
            items.push(item.clone());
        }
        added = added_next;
        added_next = Vec::new();
    }
}

pub fn panic_insert(map: &mut HashMap<String, Action>, identifier: String, action: Action) {
    if let Some(act) = map.get(&identifier) {
        if act != &action {
            println!("Conflict, not LR: tried to replace {} with {}", act, action);
        }
    }
    map.insert(identifier, action);
}

pub fn lr_parsing_table(tm: &RuleTranslationMap, lr_items: &Vec<HashSet<Item>>,
                        nonterminals: &Vec<String>) -> Box<LRTable> {
    let mut table = Box::new(LRTable {
        actions: vec![]
    });

    for (i, ref items) in lr_items.iter().enumerate() {
        table.actions.push(HashMap::new());

        for item in *items {
            println!("{} {}", i, ItemWithTr(&tm, &item));
            let rule = &tm.rules[&item.index].data[item.subindex].1;
            if rule.len() > item.position {
                if !rule[item.position].terminal {
                    continue;
                }
                let mut goto_items = HashSet::new();
                goto(&tm, &mut goto_items, &items,
                     rule[item.position].full_path.to_string());
                if let Some(pos) = lr_items.iter().position(|ref x| x == &&goto_items) {
                    panic_insert(&mut table.actions[i],
                                 rule[item.position].full_path.to_string(),
                                 Action::Shift(pos));
                }
            } else {
                if item.index == "S" {
                    panic_insert(&mut table.actions[i], "$".to_string(), Action::Accept);
                } else {
                    panic_insert(&mut table.actions[i],
                                 item.lookahead.to_string(),
                                 Action::Reduce(item.index.to_string(), item.subindex));
                }
            }
        }

        for symbol in nonterminals {
            let mut goto_items = HashSet::new();
            goto(&tm, &mut goto_items, &items, symbol.to_string());

            if let Some(pos) = lr_items.iter().position(|ref x| x == &&goto_items) {
                table.actions[i].insert(symbol.to_string(), Action::Goto(pos));
            }
        }
    }

    return table;
}

pub fn compute_lalr(tm: RuleTranslationMap, parser_items: Vec<Rule>, terminals: HashSet<(String, String)>, output: SmallVec<[P<ast::Item>; 1]>)
        -> Box<(dyn MacResult + 'static)> {
    for item in parser_items {
        println!("{}", item);
    }

    println!("\nFirst S:");
    let mut set = HashSet::new();
    first(&tm, &mut set, "S");
    println!("{:?}", set);

    let mut closure_items = HashSet::new();
    closure_items.insert(Item {
        index: "S".to_string(),
        subindex: 0,
        position: 0,
        lookahead: "$".to_string()
    });
    closure(&tm, &mut closure_items);
    println!("\nClosure:");
    for item in &closure_items {
        println!("{}", ItemWithTr(&tm, item));
    }

    let mut rule_names: Vec<String> = tm.rules.iter().map(|(x, _)| x.clone()).collect();
    rule_names.sort_unstable_by(rule_name_compare);

    for rule in &rule_names {
        println!("\nGoto {}:", rule);
        let mut goto_items = HashSet::new();
        goto(&tm, &mut goto_items, &closure_items, rule.to_string());
        let mut goto_items_vec: Vec<&Item> = goto_items.iter().collect();
        goto_items_vec.sort_unstable();
        for item in goto_items_vec {
            println!("{}", ItemWithTr(&tm, &item));
        }
    }

    let mut terminal_names: Vec<String> = terminals.iter().map(|(x, _)| x.clone()).collect();
    let mut terminal_full_names: Vec<String> = terminals.iter().map(|(_, x)| x.clone()).collect();
    terminal_names.sort_unstable_by(rule_name_compare);
    terminal_full_names.sort_unstable_by(rule_name_compare);

    for terminal in &terminal_names {
        println!("\nGoto {}:", terminal);
        let mut goto_items = HashSet::new();
        goto(&tm, &mut goto_items, &closure_items, terminal.to_string());
        for item in goto_items {
            println!("{}", ItemWithTr(&tm, &item));
        }
    }

    let mut lr_items = Vec::new();
    let mut symbols: Vec<String> = rule_names.iter().chain(terminal_full_names.iter()).map(|x| x.clone()).collect();
    symbols.sort_unstable_by(rule_name_compare);
    items(&tm, &mut lr_items, &symbols);

    println!("LR(1) items:");
    for set in &lr_items {
        let mapped_set: Vec<ItemWithTr> = set.iter().map(|value| ItemWithTr(&tm, &value)).collect();
        println!("[");
        for item in &mapped_set {
            println!("  {}", item);
        }
        println!("]");
    }

    let table = lr_parsing_table(&tm, &lr_items, &rule_names);
    print!("   ");
    for symbol in terminal_names.iter().chain(vec!["$".to_string()].iter()).chain(rule_names.iter()) {
        print!("{: >5}", &symbol[symbol.len().saturating_sub(3)..symbol.len()]);
    }
    println!("");
    for (i, row) in table.actions.iter().enumerate() {
        print!("{: <3}", i);
        for symbol in terminal_full_names.iter()
                .chain(vec!["$".to_string()].iter())
                .chain(rule_names.iter()) {
            match row.get(symbol) {
                Some(action) => print!("{}", action),
                None => print!("{}", Action::Error),
            }
        }
        println!("");
    }

    return MacEager::items(output)
}

//#[cfg(test)]
//mod tests {
//    #[test]
//    fn first() {
//        println!("First");
//    }
//}
