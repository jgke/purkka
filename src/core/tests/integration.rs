use preprocessor::PreprocessorOptions;
use core::parse_files;

fn parse(content: &str) -> Result<cparser::parser::S, Option<ctoken::token::Token>> {
    let input = "main.c";
    let get_file = |is_local, current_file, filename: String| {
        if filename == input {
            (content.to_string(), filename)
        } else {
            panic!("Unexpected include: {}", filename)
        }
    };

    parse_files(
        vec!["input.c"],
        get_file,
        PreprocessorOptions {
            include_path: vec![],
            include_files: vec![],
            definitions: vec![]
        })[0]
}

#[test]
fn fn_with_block_inside() {
    assert_eq!(parse("
int main() {
    {
    }
}
"), Err(None));
}
