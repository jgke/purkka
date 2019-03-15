extern crate clap;

extern crate parser;
extern crate preprocessor;
extern crate shared;

use clap::{Arg, App};

mod parsetree;

fn main() {
    let matches = App::new("kielic")
                          .version("0.0.1a")
                          .author("Jaakko H. <jgke@jgke.fi>")
                          .about("Compile kieli files (in theory)")
                          .arg(Arg::with_name("include")
                               .short("I")
                               .value_name("include_path")
                               .help("Sets a custom config file")
                               .multiple(true)
                               .takes_value(true))
                          .arg(Arg::with_name("input")
                               .help("Sets the input file to use")
                               .required(true)
                               .multiple(true))
                          .get_matches();

    let input: Vec<String> = matches.values_of("input").map(|v| v.into_iter().map(|i| i.to_string()).collect()).unwrap();
    let includes: Vec<String> = matches.values_of("include").map(|v| v.into_iter().map(|i| i.to_string()).collect()).unwrap_or(Vec::new());

    for file in input {
        let result = preprocessor::preprocess_file(&file, includes.iter().map(|t| t.as_ref()).collect());
        match result {
            Ok((output, context)) => {
                let parsed = parser::parse(output, &context);
                println!("{:?}", parsed);
            }
            Err(e) => panic!(e),
        }
    }
}
