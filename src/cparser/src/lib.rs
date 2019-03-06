#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

extern crate ctoken;
extern crate shared;

pub mod parser;

use std::collections::HashSet;

use parser::driver;
use ctoken::token::Token;

pub fn parse(input: Vec<Token>) -> Option<parser::S> {
    let mut types = HashSet::new();
    driver(&mut input.iter(), &mut types)
}
