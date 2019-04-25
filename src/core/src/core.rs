use std::fs::File;
use std::io::prelude::*;
use std::path;

use clap::{App, Arg};

use debug::debug::{if_debug, DebugVal::IncludeName};
use preprocessor::PreprocessorOptions;

static DEFAULT_INCLUDE_PATH: &[&str] = &[
    "/usr/local/include",
    "/usr/lib/gcc/x86_64-linux-gnu/7/include",
    "/usr/include",
];

pub fn parse_files<CB>(
    inputs: &Vec<String>,
    get_file: CB,
    options: &PreprocessorOptions,
) -> Vec<Result<(cparser::parser::S, Option<purkkaconverter::Context>), Option<ctoken::token::Token>>>
where
    CB: Copy + Fn(bool, String, String) -> (String, String),
{
    let mut res = Vec::new();
    for file in inputs {
        if file.to_lowercase().ends_with(".prk") {
            let (filename, content) = get_file(true, ".".to_string(), file.to_string());
            let (parsed, context) = purkkaconverter::convert(purkkaparser::parse_file(&filename, &content));
            res.push(Ok((parsed, Some(context))));
        } else {
            let result = preprocessor::preprocess_file(&file, get_file, options);
            match result {
                Ok((output, context)) => {
                    let parsed = parser::parse(output, &context);
                    println!("{:?}", parsed);
                    res.push(parsed.map(|p| (p, None)));
                }
                Err(e) => panic!(e),
            }
        }
    }
    res
}

pub fn real_main() {
    let matches = App::new("purkka")
        .version("0.0.1a")
        .author("Jaakko H. <jgke@jgke.fi>")
        .about("Compile purkka (.prk) files (in theory)")
        .arg(
            Arg::with_name("include")
                .short("I")
                .value_name("include path")
                .help("Add this path to the include path")
                .multiple(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("include_file")
                .short("i")
                .value_name("include file")
                .help("Add this file contents to C macro definitions")
                .multiple(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("define")
                .short("D")
                .value_name("define[=value]")
                .help("Add this file contents to C macro definitions")
                .multiple(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .value_name("output file")
                .help("Output files for prk [output].c/[output].h")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("input")
                .help("Sets the input file to use")
                .required(true)
                .multiple(true),
        )
        .get_matches();

    let input: Vec<String> = matches
        .values_of("input")
        .map(|v| v.into_iter().map(|i| i.to_string()).collect())
        .unwrap();

    let output: Option<String> = matches
        .value_of("input")
        .map(|v| v.to_string());

    let include_path: Vec<String> = matches
        .values_of("include")
        .map(|t| t.collect())
        .unwrap_or(Vec::new())
        .into_iter()
        .map(|i| i.to_string())
        .chain(DEFAULT_INCLUDE_PATH.iter().map(|t| t.to_string()))
        .collect();

    let include_files: Vec<String> = matches
        .values_of("include_file")
        .map(|v| v.into_iter().map(|i| i.to_string()).collect())
        .unwrap_or(Vec::new());

    let definitions: Vec<(String, String)> = matches
        .values_of("define")
        .map(|v| {
            v.into_iter()
                .map(|i| {
                    let split = i.split("=").collect::<Vec<_>>();
                    (
                        split[0].to_string(),
                        split.get(1).unwrap_or(&"1").to_string(),
                    )
                })
                .collect()
        })
        .unwrap_or(Vec::new());

    let options = PreprocessorOptions {
        include_path: include_path.iter().map(|t| t.as_ref()).collect(),
        include_files: include_files.iter().map(|t| t.as_ref()).collect(),
        definitions: definitions
            .iter()
            .map(|(a, b)| (a.as_ref(), b.as_ref()))
            .collect(),
    };

    let get_file = |is_local, current_file, filename: String| {
        let mut contents = String::new();
        if_debug(IncludeName, || {
            println!(
                "opening {} from {}, local: {}",
                filename, current_file, is_local
            )
        });
        if is_local {
            let mut path = path::PathBuf::from(current_file);
            path.pop();
            path.push(filename.clone());
            let full_path = path.clone();
            if let Ok(mut f) = File::open(path) {
                f.read_to_string(&mut contents)
                    .expect("something went wrong reading the file");
                return (contents, full_path.to_str().unwrap().to_string());
            }
        }
        for std_path in &options.include_path {
            let mut path = path::PathBuf::from(std_path);
            path.push(filename.clone());
            let full_path = path.clone();
            if let Ok(mut f) = File::open(path) {
                f.read_to_string(&mut contents)
                    .expect("something went wrong reading the file");
                return (contents, full_path.to_str().unwrap().to_string());
            }
        }
        panic!("File {} not found", filename);
    };

    if input.len() > 1 {
        assert!(output.is_none());
    }

    let results = parse_files(&input, get_file, &options);

    for file in results {
    }
}
