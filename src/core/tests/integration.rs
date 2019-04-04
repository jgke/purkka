use preprocessor::PreprocessorOptions;

use core::core::parse_files;

fn parse(content: &str) -> Result<cparser::parser::S, Option<ctoken::token::Token>> {
    let input = "main.c";
    let get_file = |_is_local, _current_file, filename: String| {
        if filename == input {
            (content.to_string(), filename)
        } else {
            panic!("Unexpected include: {}", filename)
        }
    };

    parse_files(
        &vec!["main.c".to_string()],
        get_file,
        &PreprocessorOptions {
            include_path: vec![],
            include_files: vec![],
            definitions: vec![]
        })[0].clone()
}

#[test]
fn fn_with_block_inside() {
    assert!(parse("
int main() {
    ({
    });
}
").is_ok());
}

#[test]
fn typedef_with_complex_expression() {
    assert!(parse("
int a = (1);
").is_ok());
}

#[test]
fn asm_weirdness() {
    assert!(parse("
#define ASM_MACRO asm ();
int main() {
    ASM_MACRO
}
").is_ok());
}

#[test]
fn va_types() {
    assert!(parse("
int main() {
    int a;
    __builtin_va_list foo;
}
").is_ok());
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
