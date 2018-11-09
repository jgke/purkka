use std::env;
use std::fs::File;
use std::io::prelude::*;

mod tokenizer;
mod tokentype;
mod parser;
mod parsetree;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut f = File::open(&args[1]).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let mut str = contents.chars();
    let mut vec = Vec::new();

    while let Some(token) = tokenizer::read_token(&mut str) {
        match token {
            tokentype::TokenType::Whitespace => {}
            _ => {
                // println!("{:?}", token);
                vec.push(token);
            }
        }
    }
    println!("{:?}", parser::parse_tokens(&mut vec.iter()));
}
