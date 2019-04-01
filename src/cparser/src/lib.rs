#![recursion_limit="100"]

#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

extern crate ctoken;

pub mod parser;

use std::collections::HashSet;

use parser::{driver, State, ScopedState};
use ctoken::token::Token;

pub fn parse(input: Vec<Token>) -> Result<parser::S, Option<Token>> {
    let mut state = State {
        scope: Vec::new(),
    };
    state.scope.push(ScopedState {
        types: HashSet::new(),
        labels: HashSet::new(),
    });
    state.scope[0].types.insert("va_list".to_string());
    state.scope[0].types.insert("__builtin_va_list".to_string());
    state.scope[0].types.insert("size_t".to_string());
    state.scope[0].types.insert("_Bool".to_string());
    state.scope[0].types.insert("_Complex".to_string());
    state.scope[0].types.insert("__label__".to_string());
    driver(&mut input.iter(), &mut state)
}
