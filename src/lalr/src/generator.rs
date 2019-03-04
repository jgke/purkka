use std::collections::HashMap;
use std::collections::HashSet;
use std::time::{Instant};
use std::env;

use types::{rule_name_compare, Action, Item, LRTable, RuleData, RuleTranslationMap};

fn first(
    tm: &RuleTranslationMap,
    set: &mut HashSet<String>,
    cache: &mut HashSet<String>,
    rule_index: &str,
) -> bool {
    if cache.get(rule_index).is_some() {
        return false;
    }
    cache.insert(rule_index.to_string());
    let rule = &tm.rules.get(rule_index).unwrap_or_else(|| panic!("No rule found for index {}", rule_index));
    let mut has_e = false;
    // E -> A | B
    for (i, _) in rule.data.iter().enumerate() {
        has_e |= first_loop(tm, set, cache, (&rule_index, i, 0));
    }
    has_e
}

fn first_loop(
    tm: &RuleTranslationMap,
    set: &mut HashSet<String>,
    cache: &mut HashSet<String>,
    rule: (&str, usize, usize),
) -> bool {
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
            let sub_has_e = first(tm, set, cache, ruledata.identifier.as_str());
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
                None => {}
                Some(RuleData { terminal: true, .. }) => {}
                Some(current_production) => {
                    let inner_production = &tm.rules.get(&current_production.identifier)
                        .unwrap_or_else(|| panic!("No rule found for {}", &current_production.identifier));
                    // for each production [B -> g] in G
                    for (i, _) in inner_production.data.iter().enumerate() {
                        let mut set = HashSet::new();
                        let mut cache = HashSet::new();
                        // for each terminal b in First(Ba)
                        if first_loop(
                            tm,
                            &mut set,
                            &mut cache,
                            (&item.index, item.subindex, item.position + 1),
                        ) {
                            added_next.insert(Item {
                                index: inner_production.identifier.clone(),
                                subindex: i,
                                position: 0,
                                lookahead: item.lookahead.to_string(),
                            });
                        }
                        for terminal in set {
                            added_next.insert(Item {
                                index: inner_production.identifier.clone(),
                                subindex: i,
                                position: 0,
                                lookahead: terminal,
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

pub fn goto(
    tm: &RuleTranslationMap,
    goto_items: &mut HashSet<Item>,
    items: &HashSet<Item>,
    rule: String,
) {
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
    initial.insert(Item {
        index: "S".to_string(),
        subindex: 0,
        position: 0,
        lookahead: "$".to_string(),
    });
    closure(&tm, &mut initial);
    println!("Closure computation done");
    items.push(initial.clone());
    let mut added = Vec::new();
    let mut added_next: Vec<HashSet<Item>> = Vec::new();
    added.push(initial);
    let mut prevsize = 0;

    while prevsize < items.len() {
        println!("Building items... {}", items.len());
        prevsize = items.len();
        for set in &added {
            for symbol in symbols {
                let mut goto_items = HashSet::new();
                goto(tm, &mut goto_items, set, symbol.to_string());
                if goto_items.len() > 0
                    && !items.contains(&goto_items)
                    && !added_next.contains(&goto_items)
                {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Core {
    index: String,
    subindex: usize,
    position: usize
}

fn get_cores(items: &HashSet<Item>) -> HashSet<Core> {
    items.iter().map(|item| Core {index: item.index.clone(), subindex: item.subindex, position: item.position}).collect()
}

pub fn get_lalr_items(lalr_items: &mut Vec<(HashSet<Item>, HashSet<Item>)>, lr_items: &Vec<HashSet<Item>>) {
    // for each set a in lr_items:
    //      set union := a
    //      for each set b != a in lr_items:
    //          if set of cores in a == set of cores in b
    //              add b to union;
    //              mark b as used;
    //      else:
    //          add a to lalr_items
    let mut used_items = HashSet::new();
    for (a, left) in lr_items.iter().enumerate() {
        if used_items.contains(&a) {
            continue;
        }
        used_items.insert(a);
        let mut union = left.clone();
        let left_cores = get_cores(left);
        for (b, right) in lr_items.iter().enumerate() {
            if used_items.contains(&b) {
                continue;
            }
            let right_cores = get_cores(right);
            if left_cores == right_cores {
                used_items.insert(b);
                for i in right.into_iter() {
                    union.insert(i.clone());
                }
            }
        }

        lalr_items.push((left.clone(), union));
    }
}

pub fn panic_insert(map: &mut HashMap<String, Action>, identifier: String, action: Action) {
    if let Some(act) = map.get(&identifier) {
        if act != &action {
            match (act, &action) {
                (Action::Reduce(_, _, _), Action::Shift(_)) =>
                    println!("Conflict, not LR: tried to replace {} with {}. Shift took precedence.", act, action),
                (Action::Shift(_), Action::Reduce(_, _, _)) => {
                    println!("Conflict, not LR: tried to replace {} with {}. Shift took precedence.", act, action);
                    return;
                },
                _ => panic!("Conflict, not LR: tried to replace {} with {}", act, action),
            }
        }
    }
    map.insert(identifier, action);
}

pub fn lr_parsing_table(
    tm: &RuleTranslationMap,
    lr_items: &Vec<(HashSet<Item>, HashSet<Item>)>,
    nonterminals: &Vec<String>,
) -> Box<LRTable> {
    let mut table = Box::new(LRTable { actions: vec![] });
    println!("Generating LR parsing table");

    let start = Instant::now();

    let total = lr_items.iter().fold(0, |total, (_, ref items)| total + items.len() * items.len());
    let mut count = 0;

    for (i, (ref _left, ref items)) in lr_items.iter().enumerate() {
        count += items.len() * items.len();
        let now = Instant::now();
        let elapsed = now.duration_since(start);
        let items_per_millisecond: f64 = (count as f64) / (elapsed.as_millis() as f64);
        let to_go: f64 = (total - count) as f64;

        println!("{} / {} ({:.1}s to go)", count, total, to_go / (items_per_millisecond * 1000.0));
        table.actions.push(HashMap::new());

        for item in items {
            let rule = &tm.rules[&item.index].data[item.subindex].1;
            if rule.len() > item.position {
                if !rule[item.position].terminal {
                    if rule[item.position].identifier == "Epsilon" {
                        panic_insert(
                            &mut table.actions[i],
                            item.lookahead.to_string(),
                            Action::Reduce(
                                item.index.to_string(),
                                item.subindex,
                                0
                            ),
                        );
                    }
                    continue;
                }
                let mut goto_items = HashSet::new();
                goto(
                    &tm,
                    &mut goto_items,
                    items,
                    rule[item.position].full_path.to_string(),
                    );
                if let Some(pos) = lr_items.iter().position(|(ref x, ref _y)| x == &goto_items) {
                    panic_insert(
                        &mut table.actions[i],
                        rule[item.position].full_path.to_string(),
                        Action::Shift(pos)
                    );
                }
            } else {
                if item.index == "S" {
                    panic_insert(&mut table.actions[i], "$".to_string(), Action::Accept);
                } else {
                    panic_insert(
                        &mut table.actions[i],
                        item.lookahead.to_string(),
                        Action::Reduce(
                            item.index.to_string(),
                            item.subindex,
                            tm.rules[&item.index].data[item.subindex].1.len(),
                        ),
                    );
                }
            }
        }

        for symbol in nonterminals {
            let mut goto_items = HashSet::new();
            goto(&tm, &mut goto_items, &items, symbol.to_string());

            if let Some(pos) = lr_items.iter().position(|(ref x, ref _y)| x == &goto_items) {
                table.actions[i].insert(symbol.to_string(), Action::Goto(pos));
            }
        }
    }

    return table;
}

pub fn compute_lalr(
    tm: &RuleTranslationMap,
    terminals: &HashSet<(String, String)>,
) -> Box<LRTable> {
    println!("Computing lalr table");
    //dbg!(tm);
    //dbg!(terminals);

    //println!("\nFirst S:");
    //let mut set = HashSet::new();
    //first(tm, &mut set, "S");
    //println!("{:?}", set);

    //let mut closure_items = HashSet::new();
    //closure_items.insert(Item {
    //    index: "S".to_string(),
    //    subindex: 0,
    //    position: 0,
    //    lookahead: "$".to_string()
    //});
    //closure(tm, &mut closure_items);
    //println!("\nClosure:");
    //for item in &closure_items {
    //    println!("{}", ItemWithTr(tm, item));
    //}

    let mut rule_names: Vec<String> = tm.rules.iter().map(|(x, _)| x.clone()).collect();
    rule_names.sort_unstable_by(rule_name_compare);

    //for rule in &rule_names {
    //    println!("\nGoto {}:", rule);
    //    let mut goto_items = HashSet::new();
    //    goto(tm, &mut goto_items, &closure_items, rule.to_string());
    //    let mut goto_items_vec: Vec<&Item> = goto_items.iter().collect();
    //    goto_items_vec.sort_unstable();
    //    for item in goto_items_vec {
    //        println!("{}", ItemWithTr(tm, &item));
    //    }
    //}

    let mut terminal_names: Vec<String> = terminals.iter().map(|(x, _)| x.clone()).collect();
    let mut terminal_full_names: Vec<String> = terminals.iter().map(|(_, x)| x.clone()).collect();
    terminal_names.sort_unstable_by(rule_name_compare);
    terminal_full_names.sort_unstable_by(rule_name_compare);

    //for terminal in &terminal_names {
    //    println!("\nGoto {}:", terminal);
    //    let mut goto_items = HashSet::new();
    //    goto(tm, &mut goto_items, &closure_items, terminal.to_string());
    //    for item in goto_items {
    //        println!("{}", ItemWithTr(tm, &item));
    //    }
    //}

    let mut lr_items = Vec::new();
    let mut symbols: Vec<String> = rule_names
        .iter()
        .chain(terminal_full_names.iter())
        .map(|x| x.clone())
        .collect();
    symbols.sort_unstable_by(rule_name_compare);
    items(tm, &mut lr_items, &symbols);
    println!("Item set building done");

    //let mut lalr_items = Vec::new();
    //get_lalr_items(&mut lalr_items, &lr_items);


    //println!("LR(1) items:");
    //for set in &lr_items {
    //    let mapped_set: Vec<ItemWithTr> = set.iter().map(|value| ItemWithTr(tm, &value)).collect();
    //    println!("[");
    //    for item in &mapped_set {
    //        println!("  {}", item);
    //    }
    //    println!("]");
    //}

    //let mut table = lr_parsing_table(tm, &lalr_items, &rule_names);
    let table = lr_parsing_table(tm,
                                 &lr_items.iter()
                                 .map(|x| (x.clone(), x.clone()))
                                 .collect(),
                                 &rule_names);
    

    //let starting_rule = &tm.rules["S"].data[0].1[0].identifier;
    //for rule in &tm.rules[starting_rule].data {
    //    if rule.1.len() == 1 && rule.1[0].identifier == "Epsilon" {
    //        table.actions[tm.indices["S"]].insert("$".to_string(), Action::Accept);
    //    }
    //}

    if env::var("DEBUG_DUMP_TABLE").is_ok() {
        println!("S: {}", tm.indices["S"]);
        println!("$: {}", tm.indices["$"]);
        print!("   ");
        for symbol in terminal_names.iter().chain(rule_names.iter()) {
            print!("{: >7.5}", &symbol);
        }
        println!("");
        for (i, row) in table.actions.iter().enumerate() {
            print!("{: <3}", i);
            for symbol in terminal_full_names.iter().chain(rule_names.iter()) {
                match row.get(symbol) {
                    Some(action) => print!("{}", action),
                    None => print!("{}", Action::Error),
                }
            }
            println!("");
        }
    }

    return table;
}
