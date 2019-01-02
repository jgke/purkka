extern crate preprocessor;
extern crate shared;

use preprocessor::{MacroToken, MacroTokenType};
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
