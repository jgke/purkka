extern crate test_generator;

use test_generator::test_resources;

use std::cell::RefCell;
use std::collections::HashSet;
use std::fs::{read_dir, File};
use std::io::prelude::*;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::Command;

use preprocessor::PreprocessorOptions;
use purkka::core::{get_default_include_path, get_file_cb, get_file_content_cb};
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

#[test_resources("src/purkka/tests/testcases/*.prk")]
fn run_test(filename: &str) {
    let filename = filename.split("/").last().unwrap();
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/testcases");

    let mut purkka_path = path.clone();
    purkka_path.push(&filename);
    let mut prk_contents = String::new();
    let mut prk = File::open(&purkka_path).unwrap();
    prk.read_to_string(&mut prk_contents).unwrap();

    let prefix = filename.split(".prk").next().unwrap();
    let mut c_path = path.clone();
    c_path.push(&format!("{}.c", &prefix));
    let mut c = File::open(&c_path).unwrap();
    let mut c_contents = String::new();
    c.read_to_string(&mut c_contents).unwrap();

    let result = parse(&prk_contents, &purkka_path.to_str().unwrap());
    assert_eq!(result.c_content, c_contents);
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
