extern crate regex;
extern crate shared;

pub mod tokenizer;
pub mod tokentype;

use std::fs::File;
use std::io::prelude::*;

use tokenizer::{MacroContext, MacroToken, ParseResult};

pub fn preprocess<CB>(get_file: CB, filename: &str) -> ParseResult<Vec<MacroToken>>
where
    CB: Fn(bool, String, String) -> String,
{
    Ok(MacroContext::new(get_file).preprocess(filename))
}

pub fn preprocess_file(filename: &str) -> ParseResult<Vec<MacroToken>> {
    let get_file = |is_local, current_file, filename| {
        let mut contents = String::new();
        let mut f = File::open(filename).expect("file not found");
        f.read_to_string(&mut contents)
            .expect("something went wrong reading the file");
        contents
    };

    preprocess(get_file, filename)
}
