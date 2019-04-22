use std::fs::{read_dir, File};
use std::path::PathBuf;
use std::process::Command;
use std::io::prelude::*;
use std::io::{self, Write};

use cformat::format_c;
use core::core::parse_files;
use preprocessor::PreprocessorOptions;

fn parse(content: &str) -> Result<cparser::parser::S, Option<ctoken::token::Token>> {
    let input = "main.prk";
    let get_file = |_is_local, _current_file, filename: String| {
        if filename == input {
            (content.to_string(), filename)
        } else {
            panic!("Unexpected include: {}", filename)
        }
    };

    parse_files(
        &vec!["main.prk".to_string()],
        get_file,
        &PreprocessorOptions {
            include_path: vec![],
            include_files: vec![],
            definitions: vec![],
        },
    )[0]
    .clone()
}

fn run_test(prefix: &str) {
    println!("Running testcase '{}'", prefix.split("/").last().unwrap());
    let mut prk_contents = String::new();
    let mut prk = File::open(format!("{}.prk", prefix)).expect("");
    prk.read_to_string(&mut prk_contents).expect("");

    let mut c_contents = String::new();
    let mut c = File::open(format!("{}.c", prefix)).expect("");
    c.read_to_string(&mut c_contents).expect("");

    assert_eq!(format_c(&parse(&prk_contents).unwrap()), c_contents);
}

#[test]
fn testcase_runner() {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("tests/testcases");
    let paths = read_dir(d).unwrap();

    for path in paths {
        let readable_path = path.unwrap().path().display().to_string();
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
                .arg("-O2")
                .arg("-o")
                .arg("/dev/null")
                .arg("-c")
                .arg(readable_path)
                .output() {
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
