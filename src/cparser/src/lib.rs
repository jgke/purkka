#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

extern crate ctoken;
extern crate shared;

pub mod parser;

use parser::driver;
use ctoken::token::Token;

pub fn parse(input: Vec<Token>) -> Option<parser::S> {
    driver(&mut input.iter())
}
