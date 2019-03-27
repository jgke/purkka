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
