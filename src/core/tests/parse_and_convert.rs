use std::cell::RefCell;
use std::collections::HashSet;

use preprocessor::PreprocessorOptions;

use core::core::get_file_cb;
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

    let ctx = RefCell::new(None);
    let res = get_file_cb(&options, &get_file_content, ctx)(FileQuery::new(".", input, true, false, HashSet::new()));
    res
}

#[test]
fn simple_constant() {
    assert!(parse(
        "
let foo: i32 = 1;
"
    )
    .is_ok());
}
