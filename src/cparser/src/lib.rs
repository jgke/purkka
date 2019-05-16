#![recursion_limit = "100"]
#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

pub mod parser;
#[macro_use]
pub mod grammar;

use ctoken::token::Token;
use fragment::fragment::{FragmentIterator, Source};

pub fn parse(input: Vec<Token>, sources: &[Source], fragment_iter: &FragmentIterator) -> Result<grammar::S, Option<Token>> {
    parser::parse(&mut input.iter().peekable(), sources, fragment_iter)
}
