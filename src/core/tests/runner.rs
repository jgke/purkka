use std::fs::{read_dir, File};
use std::path::PathBuf;
use std::process::Command;
use std::io::prelude::*;
use std::io::{self, Write};

use core::core::get_file_cb;
use preprocessor::PreprocessorOptions;
use resolve::*;

fn parse(content: &str) -> ResolveResult {
    let input = "main.prk";
    let get_file_content = |req: &FileQuery| {
        if req.requested_file == input {
            (content.to_string(), req.requested_file.clone())
        } else {
            panic!("Unexpected include: {}", req.requested_file)
        }
    };

    let options = PreprocessorOptions {
        include_path: vec![],
        include_files: vec![],
        definitions: vec![],
    };

    let res = get_file_cb(&options, &get_file_content)(FileQuery::new(".", input, true, false));
    res
}

fn run_test(prefix: &str) {
    println!("Running testcase '{}'", prefix.split("/").last().unwrap());
    let mut prk_contents = String::new();
    let mut prk = File::open(format!("{}.prk", prefix)).expect("");
    prk.read_to_string(&mut prk_contents).expect("");

    let mut c_contents = String::new();
    let mut c = File::open(format!("{}.c", prefix)).expect("");
    c.read_to_string(&mut c_contents).expect("");

    let result = parse(&prk_contents);
    assert_eq!(result.c_content, c_contents);
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
