use purkkaparser::parse_file;
use purkkaparser::parser::*;
use purkkaparser::token::Token;

use purkkatypes::{TypeSignature, Param};

fn test_parse_file(s: &str) -> S {
    parse_file("file.prk", s)
}

#[test]
fn parse_simple() {
    parse(
        &mut vec![
            Token::Let(),
            Token::Identifier(From::from("foo")),
            Token::SemiColon(),
        ]
        .iter()
        .peekable(),
    );
}

#[test]
fn parse_simple_str() {
    test_parse_file("let foo = 1;");
    test_parse_file("let foo = 1 + 2;");
    test_parse_file("let foo = 1 + 2 * 3;");
    test_parse_file("let foo = 1 * 2 + 3;");
}

#[test]
fn parse_if_else() {
    test_parse_file("let foo = if 1 + 2 { something(); } elif 3 { something_else(); } else {};");
}

fn parse_ty(s: &str) -> S {
    test_parse_file(&format!("let foo: {};", s))
}

fn get_ty(s: &S) -> Option<&TypeSignature> {
    Some(s)
        .translation_unit()
        .units()
        .and_then(|t| t.get(0))
        .declaration()
        .ty()
}

#[test]
fn parse_types() {
    parse_ty("int");
    parse_ty("int -> int");
    parse_ty("(a: int) -> int");
    parse_ty("(a: int, b: int) -> int");
    parse_ty("(a: int, b: int, c: int) -> int");
    parse_ty("(int) -> int");
    parse_ty("(int, int) -> int");
    parse_ty("(int, int, int) -> int");
    parse_ty("(a: int, int, c: int) -> int");
    parse_ty("(a: (int) -> int) -> int");
    parse_ty("((int, int)) -> int");
    parse_ty("struct { foo: int }");
    parse_ty("struct { foo: int } -> int");
    parse_ty("struct { foo: int, bar: int }");
    parse_ty("struct { foo: int, bar: struct { foo: int }, baz: int }");
    parse_ty("enum { foo }");
    parse_ty("enum { foo, bar, baz }");
    parse_ty("enum { foo(int) }");
    parse_ty("enum { foo(int, int) }");
    parse_ty("enum { foo((int, int)) } -> (int, int)");
    parse_ty("[int]");
    parse_ty("[int] -> int");
    parse_ty("[int;5]");
    parse_ty("(foo: int -> int) -> int");
    parse_ty("(int -> int) -> int");
    parse_ty("int -> int -> int");
    assert_eq!(
        get_ty(&parse_ty("&&int -> int")).unwrap(),
        &TypeSignature::Function(
            vec![Param::Anon(Box::new(TypeSignature::Pointer {
                nullable: false,
                ty: Box::new(TypeSignature::Pointer {
                    nullable: false,
                    ty: Box::new(TypeSignature::Plain(From::from("int")))
                })
            }))],
            Box::new(TypeSignature::Plain(From::from("int")))
        )
    );
}

#[test]
fn parse_nested_if_else() {
    test_parse_file(
        "let foo = if 1 + 2 {
        if 1 {
            something();
        }
    } elif 3 {
        something_else();
    } else {};",
    );
}

#[test]
fn parse_main() {
    let fun = test_parse_file(
        "fun main(argc: int, argv: [[char]]) -> int {
};",
    );
    let lambda = test_parse_file(
        "const main = fun (argc: int, argv: [[char]]) -> int {
};",
    );
    assert_eq!(fun, lambda);
}
