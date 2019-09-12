use std::cell::RefCell;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::path;

use clap::{App, Arg};
use tool::fix;

use debug::debug::{debug_p, if_debug, DebugVal::Core, DebugVal::IncludeName};
use preprocessor::{
    tokenizer::{Macro, MacroContext},
    PreprocessorOptions,
};
use resolve::{FileQuery, ResolveResult};

pub fn get_default_include_path() -> Vec<String> {
    use std::process::Command;

    let cmd = Command::new("sh")
        .arg("-c")
        .arg(r#"cpp -v /dev/null 2>&1 | sed -n '/^#include <...> search starts here:$/,/^End of search list.$/p' |head -n -1 |tail -n +2"#)
        .output()
        .expect("ls command failed to start");

    assert!(cmd.status.success());

    let stdout = String::from_utf8_lossy(&cmd.stdout);

    stdout.lines().map(|t| t.trim().to_string()).collect()
}

pub fn get_file_cb<'a>(
    options: &'a PreprocessorOptions,
    get_file_content: &'a dyn Fn(&FileQuery) -> (String, String),
    ctx: RefCell<Option<MacroContext>>,
) -> impl Fn(FileQuery) -> ResolveResult + 'a {
    fix(move |get_file: &dyn Fn(FileQuery) -> ResolveResult, req| {
        let (content, full_path) = get_file_content(&req);

        if req.need_raw {
            return ResolveResult::new_raw(&full_path, &content);
        }

        if full_path.to_lowercase().ends_with(".prk") {
            let (prk_tree, operators, symbols) = purkkaparser::parse_file(
                &full_path,
                &content,
                get_file,
                &|expansion, types| {
                    debug_p(Core, "Expanding Purkka->C string macro");
                    let result = preprocessor::preprocess_str(
                        &expansion,
                        get_file,
                        &options,
                        &mut ctx.borrow_mut(),
                    );
                    match result {
                        Ok(output) => {
                            let iter_ref = ctx.borrow();
                            let iter = iter_ref.as_ref().and_then(|c| c.iter.as_ref()).unwrap();
                            debug_p(Core, "Parsing result as C");
                            let parsed =
                                cparser::parse_macro_expansion(output, iter, types).unwrap();
                            debug_p(Core, "Converting to Purkka");
                            purkkaconverter::macros_to_purkka(parsed)
                        }
                        Err(e) => panic!(e),
                    }
                },
                &|macro_name, args, types| {
                    debug_p(Core, "Converting arguments to C");
                    let c_args: Vec<String> = args
                        .into_iter()
                        .map(purkkaconverter::expression_to_c)
                        .map(|t| cformat::expression(&t))
                        .collect();

                    let expansion = format!("{}({})", macro_name, c_args.join(", "));

                    debug_p(Core, "Preprocessing Purkka->C macro");
                    let result = preprocessor::preprocess_str(
                        &expansion,
                        get_file,
                        &options,
                        &mut ctx.borrow_mut(),
                    );
                    match result {
                        Ok(output) => {
                            let iter_ref = ctx.borrow();
                            let iter = iter_ref.as_ref().and_then(|c| c.iter.as_ref()).unwrap();
                            debug_p(Core, "Parsing result as C");
                            let parsed =
                                cparser::parse_macro_expansion(output, iter, types).unwrap();
                            debug_p(Core, "Converting result to Purkka");
                            purkkaconverter::macros_to_purkka(parsed)
                        }
                        Err(e) => panic!(e),
                    }
                },
            );
            let (declarations, types) = purkkaparser::get_declarations(&prk_tree);
            let (parsed, context) = purkkaconverter::convert(prk_tree, operators, symbols);
            let formatted = cformat::format_c(&parsed, context.local_includes);
            ResolveResult {
                full_path,
                c_content: formatted,
                h_content: None,
                dependencies: None,
                declarations: Some(declarations),
                types: Some(types),
                c_macros: (HashSet::new(), HashSet::new()),
            }
        } else {
            debug_p(Core, "Preprocessing content");
            let result = preprocessor::preprocess_file(
                &full_path,
                get_file,
                &options,
                &mut ctx.borrow_mut(),
            );

            match result {
                Ok(output) => {
                    let iter_ref = ctx.borrow();
                    let iter = iter_ref.as_ref().and_then(|c| c.iter.as_ref()).unwrap();
                    debug_p(Core, "Parsing as C");
                    let parsed = cparser::parse(output, iter, req.types).unwrap();
                    debug_p(Core, "Formatting output as C");
                    let formatted = cformat::format_c(&parsed, HashSet::new());
                    //let converted = purkkaconverter::to_purkka(parsed);
                    //let (declarations, types) = purkkaparser::get_declarations(&converted);
                    let (declarations, types) = cparser::get_declarations(&parsed);
                    let mut c_macros = (HashSet::new(), HashSet::new());
                    for (k, v) in &iter_ref.as_ref().unwrap().symbols {
                        match v {
                            Macro::Text(..) => c_macros.0.insert(k.clone()),
                            Macro::Function(..) => c_macros.1.insert(k.clone()),
                        };
                    }
                    ResolveResult {
                        full_path,
                        c_content: formatted,
                        h_content: None,
                        dependencies: None,
                        declarations: Some(declarations),
                        types: Some(types),
                        c_macros,
                    }
                }
                Err(e) => panic!(e),
            }
        }
    })
}

pub fn get_file_content_cb<'a>(
    options: &'a PreprocessorOptions,
) -> impl Fn(&FileQuery) -> (String, String) + 'a {
    move |req: &FileQuery| -> (String, String) {
        let mut contents = String::new();
        if_debug(IncludeName, || {
            println!(
                "opening {} from {}, local: {}",
                req.requested_file, req.current_file, req.local_file
            )
        });

        let mut next = req.include_next;

        if req.local_file {
            let mut path = path::PathBuf::from(req.current_file.clone());
            path.pop();
            path.push(req.requested_file.clone());
            let full_path = path.clone();
            let path_str = full_path.to_str().unwrap().to_string();
            if !next {
                if let Ok(mut f) = File::open(path) {
                    f.read_to_string(&mut contents)
                        .expect("something went wrong reading the file");
                    return (contents, path_str);
                }
            }
            if path_str == req.current_file {
                next = false;
            }
        }
        let mut cur_path = path::PathBuf::from(req.current_file.clone());
        cur_path.pop();
        let cur_path_str = cur_path.to_str().unwrap();
        for std_path in &options.include_path {
            let mut path = path::PathBuf::from(std_path);
            path.push(req.requested_file.clone());
            let full_path = path.clone();
            let path_str = full_path.to_str().unwrap().to_string();
            if !next {
                if let Ok(mut f) = File::open(path) {
                    f.read_to_string(&mut contents)
                        .expect("something went wrong reading the file");
                    return (contents, path_str);
                }
            }
            if &cur_path_str == std_path {
                next = false;
            }
        }

        panic!("File {} not found", req.requested_file);
    }
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

    let output: Option<String> = matches.value_of("output").map(ToString::to_string);

    let include_path: Vec<String> = matches
        .values_of("include")
        .map(Iterator::collect::<Vec<_>>)
        .unwrap_or_default()
        .into_iter()
        .map(ToString::to_string)
        .chain(get_default_include_path().into_iter())
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

    let get_file_content = get_file_content_cb(&options);
    let ctx = RefCell::new(None);
    let get_file = get_file_cb(&options, &get_file_content, ctx);

    for file in &input {
        let result = get_file(FileQuery::new(".", file, true, false, HashSet::new()));
        println!("{:?}", result);
        if let Some(file) = &output {
            let path = path::PathBuf::from(file);
            if File::open(path.clone()).is_ok() {
                panic!("File {} exists, exiting", file);
            } else {
                let mut file = File::create(path).unwrap();
                file.write_all(result.c_content.as_bytes()).unwrap();
            }
        }
    }
}
