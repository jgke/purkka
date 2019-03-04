#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

extern crate lalr_runtime;
extern crate preprocessor;
extern crate shared;

mod parser;
mod token;

use preprocessor::macrotoken::MacroToken;
use parser::driver;
use token::{Token, preprocessor_to_parser};

pub fn parse(input: Vec<MacroToken>) -> Option<parser::S> {
    let mut tokens: Vec<Token> = input.iter().map(|t| preprocessor_to_parser(&t.ty)).collect();
    driver(&mut tokens.iter())
}
