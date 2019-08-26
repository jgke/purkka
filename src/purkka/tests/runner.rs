use std::cell::RefCell;
use std::collections::HashSet;
use std::fs::{read_dir, File};
use std::io::prelude::*;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::Command;

use purkka::core::{get_default_include_path, get_file_cb, get_file_content_cb};
use preprocessor::PreprocessorOptions;
use resolve::*;

fn parse(content: &str, filename: &str) -> ResolveResult {
    let path = get_default_include_path();
    let options = PreprocessorOptions {
        include_path: path.iter().map(|x| x.as_ref()).collect(),
        include_files: Vec::new(),
        definitions: Vec::new(),
    };
    let cb = get_file_content_cb(&options);
    let get_file_content = |req: &FileQuery| match req.requested_file.as_str() {
        t if t == filename => (content.to_string(), req.requested_file.clone()),
        _ => cb(req),
    };

    let options = PreprocessorOptions {
        include_path: vec![],
        include_files: vec![],
        definitions: vec![],
    };
    let ctx = RefCell::new(None);
    let res = get_file_cb(&options, &get_file_content, ctx)(FileQuery::new(
        ".",
        filename,
        true,
        false,
        HashSet::new(),
    ));
    res
}

fn run_test(prefix: &str) {
    println!("Running testcase '{}'", prefix.split("/").last().unwrap());
    let mut prk_contents = String::new();
    let filename = format!("{}.prk", prefix);
    let mut prk = File::open(&filename).expect("");
    prk.read_to_string(&mut prk_contents).expect("");

    let mut c_contents = String::new();
    let mut c = File::open(format!("{}.c", prefix)).expect("");
    c.read_to_string(&mut c_contents).expect("");

    let result = parse(&prk_contents, &filename);
    assert_eq!(result.c_content, c_contents);
    println!("Ok!");
}

#[test]
fn testcase_runner() {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("tests/testcases");
    let mut paths = read_dir(d).unwrap().map(|r| r.unwrap()).collect::<Vec<_>>();
    paths.sort_by_key(|dir| dir.path());

    for path in paths {
        let readable_path = path.path().display().to_string();
        if readable_path.ends_with(".prk") {
            let mut parts = readable_path.split(".prk");
            run_test(parts.next().unwrap());
        }
    }
}

#[test]
fn check_result_files() {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("tests/testcases");
    let paths = read_dir(d).unwrap();

    for path in paths {
        let readable_path = path.unwrap().path().display().to_string();
        if readable_path.ends_with(".c") {
            let output = match Command::new("gcc")
                .arg("-Wall")
                .arg("-Wextra")
                .arg("-Werror")
                .arg("-Wno-unused-parameter")
                .arg("-Wno-unused-variable")
                .arg("-fopenmp")
                .arg("-O2")
                .arg("-o")
                .arg("/dev/null")
                .arg("-c")
                .arg(readable_path)
                .output()
            {
                Ok(ok) => ok,
                Err(e) => {
                    if let io::ErrorKind::NotFound = e.kind() {
                        return;
                    }
                    panic!("Failed to compile a C test case: {:?}", e);
                }
            };
            io::stdout().write_all(&output.stdout).unwrap();
            io::stdout().write_all(&output.stderr).unwrap();
            assert!(output.status.success());
        }
    }
}
