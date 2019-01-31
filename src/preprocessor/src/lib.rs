extern crate regex;
extern crate shared;

pub mod macrotoken;
pub mod tokenizer;
pub mod tokentype;
pub mod calculator;

use std::fs::File;
use std::io::prelude::*;
use std::path;

use macrotoken::MacroToken;
use tokenizer::{MacroContext, ParseResult};

static INCLUDE_PATH: &[&str] = &["/usr/local/include", "/usr/include"];

pub fn preprocess<CB>(get_file: CB, filename: &str) -> ParseResult<Vec<MacroToken>>
where
    CB: Fn(bool, String, String) -> (String, String),
{
    Ok(MacroContext::new(get_file).preprocess(filename))
}

pub fn preprocess_file(filename: &str) -> ParseResult<Vec<MacroToken>> {
    let get_file = |is_local, current_file, filename: String| {
        let mut contents = String::new();
        if is_local {
            let mut path = path::PathBuf::from(current_file);
            path.pop();
            path.push(filename.clone());
            let full_path = path.clone();
            if let Ok(mut f) = File::open(path) {
                f.read_to_string(&mut contents)
                    .expect("something went wrong reading the file");
                return (contents, full_path.to_str().unwrap().to_string())
            }
        }
        for std_path in INCLUDE_PATH {
            let mut path = path::PathBuf::from(std_path);
            path.push(filename.clone());
            let full_path = path.clone();
            if let Ok(mut f) = File::open(path) {
                f.read_to_string(&mut contents)
                    .expect("something went wrong reading the file");
                return (contents, full_path.to_str().unwrap().to_string())
            }
        }
        panic!("File {} not found", filename);
    };

    preprocess(get_file, filename)
}
