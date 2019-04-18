use preprocessor::PreprocessorOptions;

use core::core::parse_files;

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

#[test]
fn simple_constant() {
    assert!(parse(
        "
let foo: int = 1;
"
    )
    .is_ok());
}
