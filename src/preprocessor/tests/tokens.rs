mod common;

use common::*;
use preprocessor::macrotoken::MacroTokenType;

fn test_char(input: &str, expected: char) {
    process(
        input,
        vec![mt(
            "foo.c",
            0,
            input.len()-1,
            MacroTokenType::Char(expected),
        )],
    );
}

#[test]
fn char() {
    test_char("'a'", 'a');
    test_char("'\\141'", 'a');
    test_char("'\\40'", ' ');
    test_char("'\\1'", '\u{1}');
}
