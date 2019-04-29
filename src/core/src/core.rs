use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::path;

use clap::{App, Arg};
use tool::fix;

use debug::debug::{if_debug, DebugVal::IncludeName};
use preprocessor::PreprocessorOptions;
use resolve::{FileQuery, ResolveResult};

static DEFAULT_INCLUDE_PATH: &[&str] = &[
    "/usr/local/include",
    "/usr/lib/gcc/x86_64-linux-gnu/7/include",
    "/usr/include",
];

pub fn get_file_cb<'a>(
    options: &'a PreprocessorOptions,
    get_file_content: &'a Fn(&FileQuery) -> (String, String),
) -> impl Fn(FileQuery) -> ResolveResult + 'a {
    fix(move |get_file: &Fn(FileQuery) -> ResolveResult, req| {
        let (content, full_path) = get_file_content(&req);

        if req.need_raw {
            return ResolveResult {
                full_path,
                c_content: content,
                h_content: None,
                dependencies: None,
                declarations: None,
            };
        }

        if full_path.to_lowercase().ends_with(".prk") {
            let (parsed, context) =
                purkkaconverter::convert(purkkaparser::parse_file(&full_path, &content));
            let formatted = cformat::format_c(&parsed, context.local_includes);
            ResolveResult {
                full_path,
                c_content: formatted,
                h_content: None,
                dependencies: None,
                declarations: None,
            }
        } else {
            let result = preprocessor::preprocess_file(&full_path, get_file, &options);
            match result {
                Ok((output, context)) => {
                    let parsed = parser::parse(output, &context);
                    let formatted = cformat::format_c(&parsed.unwrap(), HashSet::new());
                    ResolveResult {
                        full_path,
                        c_content: formatted,
                        h_content: None,
                        dependencies: None,
                        declarations: None,
                    }
                }
                Err(e) => panic!(e),
            }
        }
    })
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
        .map(|v| v.map(ToString::to_string).collect())
        .unwrap();

    let output: Option<String> = matches.value_of("input").map(ToString::to_string);

    let include_path: Vec<String> = matches
        .values_of("include")
        .map(Iterator::collect::<Vec<_>>)
        .unwrap_or_default()
        .into_iter()
        .map(ToString::to_string)
        .chain(DEFAULT_INCLUDE_PATH.iter().map(ToString::to_string))
        .collect();

    let include_files: Vec<String> = matches
        .values_of("include_file")
        .map(|v| v.map(ToString::to_string).collect())
        .unwrap_or_default();

    let definitions: Vec<(String, String)> = matches
        .values_of("define")
        .map(|v| {
            v.map(|i| {
                let split = i.split('=').collect::<Vec<_>>();
                (
                    split[0].to_string(),
                    split.get(1).unwrap_or(&"1").to_string(),
                    )
            })
            .collect()
        })
        .unwrap_or_default();

    if input.len() > 1 {
        assert!(output.is_none());
    }

    let options = PreprocessorOptions {
        include_path: include_path.iter().map(AsRef::as_ref).collect(),
        include_files: include_files.iter().map(AsRef::as_ref).collect(),
        definitions: definitions
            .iter()
            .map(|(a, b)| (a.as_ref(), b.as_ref()))
            .collect(),
    };

    let get_file_content = |req: &FileQuery| -> (String, String) {
        let mut contents = String::new();
        if_debug(IncludeName, || {
            println!(
                "opening {} from {}, local: {}",
                req.requested_file, req.current_file, req.local_file
            )
        });
        if req.local_file {
            let mut path = path::PathBuf::from(req.current_file.clone());
            path.pop();
            path.push(req.requested_file.clone());
            let full_path = path.clone();
            if let Ok(mut f) = File::open(path) {
                f.read_to_string(&mut contents)
                    .expect("something went wrong reading the file");
                return (contents, full_path.to_str().unwrap().to_string());
            }
        }
        for std_path in &options.include_path {
            let mut path = path::PathBuf::from(std_path);
            path.push(req.requested_file.clone());
            let full_path = path.clone();
            if let Ok(mut f) = File::open(path) {
                f.read_to_string(&mut contents)
                    .expect("something went wrong reading the file");
                return (contents, full_path.to_str().unwrap().to_string());
            }
        }

        panic!("File {} not found", req.requested_file);
    };

    let get_file = get_file_cb(&options, &get_file_content);
    for file in &input {
        println!("{:?}", get_file(FileQuery::new(".", file, true, false)))
    }
}
