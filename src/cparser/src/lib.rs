#![recursion_limit="100"]

#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

extern crate ctoken;

pub mod parser;

use std::collections::HashSet;

use parser::driver;
use ctoken::token::Token;

pub fn parse(input: Vec<Token>) -> Result<parser::S, Option<Token>> {
    let mut types = HashSet::new();
    types.insert("va_list".to_string());
    types.insert("size_t".to_string());
    types.insert("_Bool".to_string());
    types.insert("_Complex".to_string());
    driver(&mut input.iter(), &mut types)
}
