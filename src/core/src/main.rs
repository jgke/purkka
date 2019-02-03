#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]


extern crate lalr_runtime;
extern crate preprocessor;
extern crate shared;

use std::env;

mod parser;
mod parsetree;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("Usage: kielic [filename]");
    } else {
        let result = preprocessor::preprocess_file(&args[1]);
        match result {
            Ok(output) => {
                for token in output {
                    println!("{:?}", token)
                }
            }
            Err(e) => panic!(e),
        }
    }
}
