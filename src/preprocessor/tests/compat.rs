mod common;

use common::*;
use preprocessor::macrotoken::{MacroTokenType, SpecialType};
use preprocessor::tokentype::Punctuation;

#[test]
fn asm_expr() {
    assert_eq!(preprocess_string(
        "main.c",
        "asm ( anything can go here [ ] ( ) )",
    ).unwrap().len(), 1);
    assert_eq!(preprocess_string(
        "main.c",
        "asm volatile inline goto ( anything can go here [ ] ( ) )",
    ).unwrap().len(), 1);
    let res = preprocess_string(
        "main.c",
        "#define ASM_MACRO asm();\nASM_MACRO",
    ).unwrap();
    assert!(if let MacroTokenType::Special(SpecialType::Asm(_)) = res[0].ty { true } else { false });
    assert!(if let MacroTokenType::Punctuation(Punctuation::Semicolon) = res[1].ty { true } else { false });
}