#![recursion_limit = "100"]
#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![plugin(kieliparser_procmacros)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

pub mod parser;
pub mod token;
pub mod tokenizer;

use parser::{parse, S};
use tokenizer::tokenize;

pub fn parse_file(content: &str, filename: &str) -> S {
    let (tokens, _interner) = tokenize(content, filename);
    parse(&mut tokens.iter().peekable())
}
