extern crate regex;
extern crate shared;

pub mod tokentype;
pub mod tokenizer;

use std::fs::File;
use std::io::prelude::*;

use shared::fragment::FragmentIterator;
use tokenizer::{MacroToken, MacroContext, ParseResult};

pub fn preprocess_string(filename: &str, content: &str) -> ParseResult<Vec<MacroToken>> {
    let mut iter = FragmentIterator::new(filename, content);
    Ok(MacroContext::new().preprocess(&mut iter))
}

pub fn preprocess_file(filename: &str) -> ParseResult<Vec<MacroToken>> {
    let mut contents = String::new();

    let mut f = File::open(filename).expect("file not found");
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    preprocess_string(filename, &contents)
}
