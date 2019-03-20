extern crate clap;

extern crate parser;
extern crate preprocessor;
extern crate shared;

use clap::{Arg, App};

mod parsetree;

static DEFAULT_INCLUDE_PATH: &[&str] = &["/usr/local/include", "/usr/lib/gcc/x86_64-linux-gnu/7/include", "/usr/include"];

fn main() {
    let matches = App::new("kielic")
                          .version("0.0.1a")
                          .author("Jaakko H. <jgke@jgke.fi>")
                          .about("Compile kieli files (in theory)")
                          .arg(Arg::with_name("include")
                               .short("I")
                               .value_name("include path")
                               .help("Add this path to the include path")
                               .multiple(true)
                               .takes_value(true))
                          .arg(Arg::with_name("include_file")
                               .short("i")
                               .value_name("include file")
                               .help("Add this file contents to macro definitions")
                               .multiple(true)
                               .takes_value(true))
                          .arg(Arg::with_name("define")
                               .short("D")
                               .value_name("define[=value]")
                               .help("Add this file contents to macro definitions")
                               .multiple(true)
                               .takes_value(true))
                          .arg(Arg::with_name("input")
                               .help("Sets the input file to use")
                               .required(true)
                               .multiple(true))
                          .get_matches();

    let input: Vec<String> = matches.values_of("input").map(|v| v.into_iter().map(|i| i.to_string()).collect()).unwrap();

    let include_path: Vec<String> = matches.values_of("include")
        .map(|t| t.collect())
        .unwrap_or(Vec::new()).into_iter()
        .map(|i| i.to_string())
        .chain(DEFAULT_INCLUDE_PATH.iter().map(|t| t.to_string()))
        .collect();

    let include_files: Vec<String> = matches.values_of("include_file").map(|v| v.into_iter().map(|i| i.to_string()).collect()).unwrap_or(Vec::new());

    let definitions: Vec<(String, String)> = matches.values_of("define").map(|v| v.into_iter().map(|i| {
        let split = i.split("=").collect::<Vec<_>>();
        (split[0].to_string(), split.get(1).unwrap_or(&"1").to_string())
    }).collect()).unwrap_or(Vec::new());

    let options = preprocessor::PreprocessorOptions {
        include_path: include_path.iter().map(|t| t.as_ref()).collect(),
        include_files: include_files.iter().map(|t| t.as_ref()).collect(),
        definitions: definitions.iter().map(|(a, b)| (a.as_ref(), b.as_ref())).collect(),
    };

    for file in input {
        let result = preprocessor::preprocess_file(&file, &options);
        match result {
            Ok((output, context)) => {
                let parsed = parser::parse(output, &context);
                println!("{:?}", parsed);
            }
            Err(e) => panic!(e),
        }
    }
}
