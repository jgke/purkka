extern crate preprocessor;
extern crate shared;

use preprocessor::tokentype::Operator;
use preprocessor::tokenizer::{MacroToken, MacroTokenType};
use shared::fragment::{FragmentIterator, Source, Span};

fn process(original: &str, expected: Vec<MacroToken>) {
    let iter = FragmentIterator::new("foo.c", original);
    let processed = preprocessor::preprocess_string("foo.c", original);
    if let Ok(p) = &processed {
        println!("---- Test result ----");
        println!("Result:");
        p.iter().for_each(|t| println!("{}", t.display(&iter)));
        println!("Expected:");
        expected.iter().for_each(|t| println!("{}", t.display(&iter)));
    }
    assert_eq!(processed, Ok(expected));
}

fn mt(file: &str, lo: usize, hi: usize, ty: MacroTokenType) -> MacroToken {
    MacroToken {
        source: s(file, lo, hi, None),
        ty
    }
}

fn mt_s(file: &str, lo: usize, hi: usize, ty: MacroTokenType, source: Option<Source>) -> MacroToken {
    MacroToken {
        source: s(file, lo, hi, source),
        ty
    }
}

fn s(file: &str, lo: usize, hi: usize, s: Option<Source>) -> Source {
    Source {
        filename: file.to_string(),
        span: Span {
            lo,
            hi,
            source: s.map(|s| Box::new(s))
        }
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

#[test]
fn simple_macro() {
    process(
        "#define FOO BAR\nFOO",
        vec![
        mt_s("foo.c", 16, 18, 
           MacroTokenType::Identifier("BAR".to_string()),
           Some(s("foo.c", 0, 15, Some(s("foo.c", 12, 14, None))))),
        ]);
}

#[test]
fn simple_macro_with_offset() {
    process(
        "\n\n#define FOO BAR\nFOO",
        vec![
        mt_s("foo.c", 18, 20, 
           MacroTokenType::Identifier("BAR".to_string()),
           Some(s("foo.c", 2, 17, Some(s("foo.c", 14, 16, None))))),
        ]);
}

#[test]
fn multiple_macros() {
    process(
        "#define BAR BAZ\n#define FOO BAR\nFOO",
        vec![
        mt_s("foo.c", 32, 34, 
           MacroTokenType::Identifier("BAZ".to_string()),
           Some(s("foo.c", 16, 31, // #define BAR BAZ
                  Some(s("foo.c", 28, 30, // BAZ
                         Some(s("foo.c", 0, 15, // #define FOO BAR
                                Some(s("foo.c", 12, 14, // BAR
                                       None))))))))),
        ]);
}

#[test]
fn recursive_macro() {
    process(
        "\n\n#define FOO FOO\nFOO",
        vec![
        mt_s("foo.c", 18, 20, 
           MacroTokenType::Identifier("FOO".to_string()),
           Some(s("foo.c", 2, 17, Some(s("foo.c", 14, 16, None))))),
        ]);
}

#[test]
fn mutually_recursive_macros() {
    process(
        "#define BAR FOO\n#define FOO BAR\nFOO",
        vec![
        mt_s("foo.c", 32, 34, 
           MacroTokenType::Identifier("FOO".to_string()),
           Some(s("foo.c", 16, 31, // #define BAR BAZ
                  Some(s("foo.c", 28, 30, // BAZ
                         Some(s("foo.c", 0, 15, // #define FOO BAR
                                Some(s("foo.c", 12, 14, // BAR
                                       None))))))))),
        ]);
}

#[test]
fn function_macro() {
    process(
        "#define FOO() foo\nFOO()",
        vec![
        mt_s("foo.c", 18, 22, 
           MacroTokenType::Identifier("foo".to_string()),
           Some(s("foo.c", 0, 17, Some(s("foo.c", 14, 16, None))))),
        ]);
}

#[test]
fn function_macro_one_arg() {
    process(
        "#define FOO(a) a\nFOO(b)",
        vec![
        mt_s("foo.c", 21, 21, // b
           MacroTokenType::Identifier("b".to_string()),
           Some(s("foo.c", 17, 22, // FOO(b)
                  Some(s("foo.c", 0, 16, // #define FOO(a) a
                         Some(s("foo.c", 15, 15, // a
                                None
                               ))))))),
        ]);
}

#[test]
fn function_macro_nested() {
    process(
        "#define BAR(a) a\n#define FOO(a) BAR(a)\nFOO(b)",
        vec![
        mt_s("foo.c", 43, 43, // b
           MacroTokenType::Identifier("b".to_string()),
           Some(s("foo.c", 39, 44, // FOO(b)
                  Some(s("foo.c", 17, 38, // #define FOO(a) BAR(a)
                         Some(s("foo.c", 32, 34, // BAR
                                Some(s("foo.c", 0, 16, // #define BAR(a) a
                                       Some(s("foo.c", 15, 15, // #define a
                                              None
                                             ))))))))))),
        ]);
}

// todo: test for eof after "#define foo" and "#define"
