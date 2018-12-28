#![feature(plugin)]
#![plugin(lalr)]

use std::env;

mod tokenizer;
mod tokentype;
mod parser;
mod parsetree;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("Usage: kielic [filename]");
    } else {
        let result = tokenizer::parse(&args[1]);
        println!("{:?}", result);
    }
}
