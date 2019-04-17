mod common;

use common::*;
use preprocessor::macrotoken::{MacroTokenType, SpecialType};
use preprocessor::tokentype::Punctuation;

#[test]
fn asm_expr() {
    assert_eq!(
        preprocess_string("main.c", "asm ( anything can go here [ ] ( ) )",)
            .unwrap()
            .len(),
        1
    );
    assert_eq!(
        preprocess_string(
            "main.c",
            "asm volatile inline goto ( anything can go here [ ] ( ) )",
        )
        .unwrap()
        .len(),
        1
    );
    assert_eq!(
        preprocess_string("main.c", "#define FOO a ## b ## c\nFOO",)
            .unwrap()
            .len(),
        1
    );
    assert_eq!(
        preprocess_string("main.c", "#define FOO(t) a ## b ## c\nFOO(_)",)
            .unwrap()
            .len(),
        1
    );
    assert_eq!(
        preprocess_string("main.c", "#define FOO(a) a ## b ## c\nFOO(a)",)
            .unwrap()
            .len(),
        1
    );
    assert_eq!(
        preprocess_string("main.c", "#define FOO bar /* baz */ \\\nqux\n",)
            .unwrap()
            .len(),
        0
    );
    assert_eq!(
        preprocess_string("main.c", "#define FOO bar /* baz */\\\nqux\n",)
            .unwrap()
            .len(),
        0
    );
    assert_eq!(
        preprocess_string("main.c", "#if !defined(FOO)\nbar\n#endif\n",)
            .unwrap()
            .len(),
        1
    );
    let res = preprocess_string("main.c", "#define ASM_MACRO asm();\nASM_MACRO").unwrap();
    assert!(
        if let MacroTokenType::Special(SpecialType::Asm(_)) = res[0].ty {
            true
        } else {
            false
        }
    );
    assert!(
        if let MacroTokenType::Punctuation(Punctuation::Semicolon) = res[1].ty {
            true
        } else {
            false
        }
    );
}
