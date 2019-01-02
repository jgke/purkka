extern crate preprocessor;
extern crate shared;

use preprocessor::tokentype::Operator;
use preprocessor::tokenizer::{MacroToken, MacroTokenType};
use shared::fragment::{Source, Span};

fn process(original: &str, expected: Vec<MacroToken>) {
    assert_eq!(
        preprocessor::preprocess_string("foo.c", original),
        Ok(expected));
}

fn mt(file: &str, lo: usize, hi: usize, ty: MacroTokenType) -> MacroToken {
    MacroToken {
        source: Source {
            filename: file.to_string(),
            span: Span {
                lo,
                hi,
                source: None
            }
        },
        ty
    }
}

#[test]
fn one_identifier() {
    process(
        "Foo",
        vec![mt("foo.c", 0, 2, MacroTokenType::Identifier("Foo".to_string()))]);
 }

#[test]
fn three_identifiers() {
    process(
        "Foo Bar Baz",
        vec![
        mt("foo.c", 0, 2, 
            MacroTokenType::Identifier("Foo".to_string())),
        mt("foo.c", 4, 6, 
            MacroTokenType::Identifier("Bar".to_string())),
        mt("foo.c", 8, 10, 
            MacroTokenType::Identifier("Baz".to_string()))]);
}

#[test]
fn strings() {
    process(
        "\"foo\" \"bar\\nbaz\"",
        vec![
        mt("foo.c", 0, 4, 
            MacroTokenType::StringLiteral("foo".to_string())),
        mt("foo.c", 6, 15, 
            MacroTokenType::StringLiteral("bar\nbaz".to_string()))]);
}

#[test]
fn numbers() {
    process(
        "1 .54e+1..a5",
        vec![
        mt("foo.c", 0, 0, 
            MacroTokenType::Number("1".to_string())),
        mt("foo.c", 2, 11, 
            MacroTokenType::Number(".54e+1..a5".to_string()))]);
}

#[test]
fn period() {
    process(
        ". .a",
        vec![
        mt("foo.c", 0, 0, 
            MacroTokenType::Operator(Operator::Dot)),
        mt("foo.c", 2, 2, 
            MacroTokenType::Operator(Operator::Dot)),
        mt("foo.c", 3, 3, 
            MacroTokenType::Identifier("a".to_string()))
        ]);
}

#[test]
fn comments() {
    process(
        "/* foo */ bar //baz \nqux // bax \\\nbux",
        vec![
        mt("foo.c", 10, 12, 
            MacroTokenType::Identifier("bar".to_string())),
        mt("foo.c", 21, 23, 
            MacroTokenType::Identifier("qux".to_string()))]);
}

#[test]
fn whitespace() {
    process(
        "\t   \t \\\n \n   ",
        vec![]);
}
