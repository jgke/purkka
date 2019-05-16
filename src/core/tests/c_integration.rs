use preprocessor::PreprocessorOptions;

use core::core::get_file_cb;

use resolve::*;

fn parse(content: &str) -> ResolveResult {
    let input = "main.c";
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

#[test]
fn fn_with_block_inside() {
    assert!(parse(
        "
int main() {
    ({
    });
}
"
    )
    .is_ok());
}

#[test]
fn initialization_expression() {
    assert!(parse(
        "
int a = (1);
"
    )
    .is_ok());
}

#[test]
fn asm_weirdness() {
    assert!(parse(
        "
#define ASM_MACRO asm ();
int main() {
    ASM_MACRO
}
"
    )
    .is_ok());
}

#[test]
fn va_types() {
    assert!(parse(
        "
int main() {
    int a;
    __builtin_va_list foo;
}
"
    )
    .is_ok());
}

#[test]
fn init_array() {
    assert!(parse(" int main() { int a[] = {}; } ").is_ok());

    assert!(parse(" int main() { int a[] = {1}; } ").is_ok());
    assert!(parse(" int main() { int a[] = {1,2}; } ").is_ok());
    assert!(parse(" int main() { int a[] = {1,2,3}; } ").is_ok());

    assert!(parse(" int main() { int a[] = {1,}; } ").is_ok());
    assert!(parse(" int main() { int a[] = {1,2,}; } ").is_ok());
    assert!(parse(" int main() { int a[] = {1,2,3,}; } ").is_ok());
}
