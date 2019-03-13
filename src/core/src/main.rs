extern crate parser;
extern crate preprocessor;
extern crate shared;

use std::env;

mod parsetree;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("Usage: kielic [filename]");
    } else {
        let result = preprocessor::preprocess_file(&args[1]);
        match result {
            Ok((output, context)) => {
                let parsed = parser::parse(output, &context);
                println!("{:?}", parsed);
            }
            Err(e) => panic!(e),
        }
    }
}
