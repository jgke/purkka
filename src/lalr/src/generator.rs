use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::atomic::AtomicUsize;
use std::sync::{atomic, Mutex};
use std::time::Instant;

use rayon::prelude::*;

use crate::types::{
    Action, Component, Core, Index, Item, LRTable, RuleData, RuleTranslationMap, Terminal,
};
use debug::debug::{if_debug, DebugVal::DumpLalrTable};

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
        has_e |= first_loop(tm, set, cache, (rule_index, i, 0));
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
            break;
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
            let Component { rules, .. } = &tm.rules[&item.index].data[item.subindex];
            match &rules.get(item.position) {
                None => {}
                Some(RuleData { terminal: true, .. }) => {}
                Some(current_production) => {
                    let inner_production = tm
                        .indices
                        .get(&current_production.identifier)
                        .and_then(|i| tm.rules.get(i))
                        .unwrap_or_else(|| {
                            panic!("No rule found for {}", &current_production.identifier)
                        });
                    // for each production [B -> g] in G
                    for (i, _) in inner_production.data.iter().enumerate() {
                        let mut set = HashSet::new();
                        let mut cache = HashSet::new();
                        // for each terminal b in First(Ba)
                        if first_loop(
                            tm,
                            &mut set,
                            &mut cache,
                            (item.index, item.subindex, item.position + 1),
                        ) {
                            added_next.insert(Item {
                                index: tm.indices[&inner_production.identifier],
                                subindex: i,
                                position: 0,
                                lookahead: item.lookahead,
                            });
                        }
                        for terminal in set {
                            added_next.insert(Item {
                                index: tm.indices[&inner_production.identifier],
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
    rule: Index,
) {
    for item in items {
        let Component { rules, .. } = &tm.rules[&item.index].data[item.subindex];
        if rules.len() > item.position && tm.indices[&rules[item.position].full_path] == rule {
            let mut goto_item = item.clone();
            goto_item.position += 1;
            goto_items.insert(goto_item);
        }
    }
    closure(tm, goto_items);
}

pub fn items(tm: &RuleTranslationMap, items: &mut Vec<HashSet<Item>>, symbols: &Vec<Index>) {
    let mut initial = HashSet::new();
    initial.insert(Item {
        index: tm.indices["S"],
        subindex: 0,
        position: 0,
        lookahead: tm.indices["$"],
    });
    closure(&tm, &mut initial);
    println!("Closure computation done");
    items.push(initial.clone());
    let mut added = Vec::new();
    let mut added_next: Vec<HashSet<Item>> = Vec::new();
    added.push(initial);
    let mut prevsize = 0;
    println!("Building items...");

    while prevsize < items.len() {
        println!("{}", prevsize);
        prevsize = items.len();
        for set in &added {
            for symbol in symbols {
                let mut goto_items = HashSet::new();
                goto(tm, &mut goto_items, set, *symbol);
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

fn get_cores(items: &HashSet<Item>) -> HashSet<Core> {
    items
        .iter()
        .map(|item| Core {
            index: item.index.clone(),
            subindex: item.subindex,
            position: item.position,
        })
        .collect()
}

pub fn get_lalr_items(lalr_items: &mut Vec<HashSet<Item>>, lr_items: &Vec<HashSet<Item>>) {
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

        lalr_items.push(union);
    }
}

pub fn panic_insert(
    rev_indices: &HashMap<Index, String>,
    map: &mut HashMap<Index, Action>,
    identifier: Index,
    action: Action,
) {
    // The map should have the rule with the following priority:
    //  1) higher priority number
    //  2) if the priorities are equal, the associativity should also be equal
    //      2a) if the associativity is left associative, shift
    //      2b) if the associativity is right associative, reduce
    if let Some(act) = map.get(&identifier) {
        if act != &action {
            match (act, &action) {
                (Action::Reduce(_, _, _, map_prio), Action::Shift(_, new_prio)) => {
                    let (map_prio, map_left_assoc) = map_prio.unwrap_or((0, false));
                    let (new_prio, new_left_assoc) = new_prio.unwrap_or((0, false));
                    match (map_prio.cmp(&new_prio), map_left_assoc, new_left_assoc) {
                        (Ordering::Less, _, _) => {}
                        (Ordering::Greater, _, _) => {
                            return;
                        }
                        (Ordering::Equal, false, false) => {
                            if map_prio == 0 {
                                println!("Warning: conflicting actions {} and {}, shift took priority.\nToken: {}",
                                         act, action, rev_indices[&identifier]);
                            }
                            // map contains reduce, so replaced it with shift
                        }
                        (Ordering::Equal, true, true) => {
                            return;
                        }
                        (Ordering::Equal, _, _) => panic!(
                            "Conflicting priority rules at {} and {}\nToken: {}",
                            act, action, rev_indices[&identifier]
                        ),
                    }
                }
                (Action::Shift(_, map_prio), Action::Reduce(_, _, _, new_prio)) => {
                    let (map_prio, map_left_assoc) = map_prio.unwrap_or((0, false));
                    let (new_prio, new_left_assoc) = new_prio.unwrap_or((0, false));
                    match (map_prio.cmp(&new_prio), map_left_assoc, new_left_assoc) {
                        (Ordering::Less, _, _) => {}
                        (Ordering::Greater, _, _) => {
                            return;
                        }
                        (Ordering::Equal, false, false) => {
                            if map_prio == 0 {
                                println!("Warning: conflicting actions {} and {}, shift took priority.\nToken: {}",
                                         act, action, rev_indices[&identifier]);
                            }
                            // map contains shift, so don't replace it
                            return;
                        }
                        (Ordering::Equal, true, true) => {
                            return;
                        }
                        (Ordering::Equal, _, _) => panic!(
                            "Conflicting priority rules at {} and {}\nToken: {}",
                            act, action, rev_indices[&identifier]
                        ),
                    }
                }
                _ => {
                    if let Action::Reduce(orig_index, orig_subindex, ..) = act {
                        if let Action::Reduce(new_index, new_subindex, ..) = action {
                            panic!(
                                "Conflict, not LR: tried to replace {}:{} with {}:{}\nToken: {:?}",
                                rev_indices[orig_index], orig_subindex,
                                rev_indices[&new_index], new_subindex,
                                rev_indices[&identifier]);
                        }
                    }
                    panic!(
                        "Conflict, not LR: tried to replace {} with {}\nToken: {:?}",
                        act, action, rev_indices[&identifier]);
                }
            }
        }
    }
    map.insert(identifier, action);
}

pub fn lr_parsing_table(
    tm: &RuleTranslationMap,
    lr_items: &Vec<HashSet<Item>>,
    nonterminals: &Vec<Index>,
) -> Box<LRTable> {
    let mut open_table = Box::new(LRTable { actions: vec![] });
    {
        let table = Mutex::new(&mut open_table);
        println!("Generating LR parsing table");

        let start = Instant::now();

        let total = lr_items
            .iter()
            .fold(0, |total, ref items| total + items.len());
        let count = AtomicUsize::new(0);
        lr_items
            .iter()
            .for_each(|_| table.lock().unwrap().actions.push(HashMap::new()));

        lr_items.par_iter().enumerate().for_each(|(i, ref items)| {
            count.fetch_add(items.len(), atomic::Ordering::Relaxed);
            let new_count = count.load(atomic::Ordering::Relaxed);
            let now = Instant::now();
            let elapsed = now.duration_since(start);
            let items_per_millisecond: f64 = (new_count as f64) / (elapsed.as_millis() as f64);
            let items_per_second: f64 = items_per_millisecond * 1000.0;
            let to_go: f64 = (total - new_count) as f64;

            if new_count > 1000 && total > 10000 {
                println!(
                    "{} / {} ({:.1}s to go @ {:.1} items/s)",
                    new_count,
                    total,
                    to_go / items_per_second,
                    items_per_second
                );
            }

            for item in items.iter() {
                let Component {
                    rules, priority, ..
                } = &tm.rules[&item.index].data[item.subindex];
                if rules.len() > item.position {
                    if !rules[item.position].terminal {
                        if rules[item.position].identifier == "Epsilon" {
                            panic_insert(
                                &tm.rev_indices,
                                &mut table.lock().unwrap().actions[i],
                                item.lookahead,
                                Action::Reduce(item.index, item.subindex, 0, *priority),
                            );
                        }
                        continue;
                    }
                    let mut goto_items = HashSet::new();
                    goto(
                        &tm,
                        &mut goto_items,
                        items,
                        tm.indices[&rules[item.position].full_path],
                    );
                    if let Some(pos) = lr_items
                        .iter()
                        .position(|x| get_cores(x) == get_cores(&goto_items))
                    {
                        panic_insert(
                            &tm.rev_indices,
                            &mut table.lock().unwrap().actions[i],
                            tm.indices[&rules[item.position].full_path],
                            Action::Shift(pos, *priority),
                        );
                    }
                } else {
                    if item.index == tm.indices["S"] {
                        panic_insert(
                            &tm.rev_indices,
                            &mut table.lock().unwrap().actions[i],
                            tm.indices["$"],
                            Action::Accept,
                        );
                    } else {
                        panic_insert(
                            &tm.rev_indices,
                            &mut table.lock().unwrap().actions[i],
                            item.lookahead,
                            Action::Reduce(
                                item.index,
                                item.subindex,
                                tm.rules[&item.index].data[item.subindex].rules.len(),
                                *priority,
                            ),
                        );
                    }
                }
            }

            for symbol in nonterminals {
                let mut goto_items = HashSet::new();
                goto(&tm, &mut goto_items, &items, *symbol);

                if let Some(pos) = lr_items
                    .iter()
                    .position(|x| get_cores(x) == get_cores(&goto_items))
                {
                    table.lock().unwrap().actions[i].insert(*symbol, Action::Goto(pos));
                }
            }
        });
    }

    return open_table;
}

pub fn compute_lalr(tm: &RuleTranslationMap, terminals: &HashSet<Terminal>) -> Box<LRTable> {
    for arg in std::env::args() {
        if arg == "--emit=dep-info,metadata" {
            println!("Running in check mode! Skipping LALR table generation.");
            return Box::new(LRTable { actions: vec![] });
        }
    }

    println!("Computing lalr table");
    let mut rule_names: Vec<Index> = tm.rules.iter().map(|(x, _)| x.clone()).collect();
    rule_names.sort_unstable();

    let mut terminal_names: Vec<Index> = terminals
        .iter()
        .map(|term| {
            tm.indices
                .get(&term.full_path)
                .map(|t| *t)
                .unwrap_or_else(|| panic!("Terminal {} not found", &term.full_path))
        })
        .collect();
    terminal_names.sort_unstable();

    let mut symbols: Vec<Index> = rule_names
        .iter()
        .chain(terminal_names.iter())
        .map(|x| *x)
        .collect();
    symbols.sort_unstable();
    let mut lr_items = Vec::new();
    items(tm, &mut lr_items, &symbols);
    println!("Item set building done");

    let mut lalr_items = Vec::new();
    get_lalr_items(&mut lalr_items, &lr_items);
    //get_lalr_items_fast(tm, &mut lalr_items, &lr_items);
    //lalr_items = lr_items;
    //let lalr_items = vec![];

    //println!("LR(1) items:");
    //for set in &lr_items {
    //    let mapped_set: Vec<ItemWithTr> = set.iter().map(|value| ItemWithTr(tm, &value)).collect();
    //    println!("[");
    //    for item in &mapped_set {
    //        println!("  {}", item);
    //    }
    //    println!("]");
    //}

    let table = lr_parsing_table(tm, &lalr_items, &rule_names);
    println!();

    //let starting_rule = &tm.rules["S"].data[0].1[0].identifier;
    //for rule in &tm.rules[starting_rule].data {
    //    if rule.1.len() == 1 && rule.1[0].identifier == "Epsilon" {
    //        table.actions[tm.indices["S"]].insert("$".to_string(), Action::Accept);
    //    }
    //}

    if_debug(DumpLalrTable, || {
        println!("Rule indices:");
        for symbol in terminal_names.iter().chain(rule_names.iter()) {
            println!("{}: {}", &symbol, tm.rev_indices[&symbol]);
        }
        println!();

        println!("LALR table");
        print!("   ");
        for symbol in terminal_names.iter().chain(rule_names.iter()) {
            print!("{: >7.5}", &symbol);
        }
        println!("");
        for (i, row) in table.actions.iter().enumerate() {
            print!("{: <3}", i);
            for symbol in terminal_names.iter().chain(rule_names.iter()) {
                match row.get(symbol) {
                    Some(action) => print!("{}", action),
                    None => print!("{}", Action::Error),
                }
            }
            println!("");
        }
    });

    return table;
}
