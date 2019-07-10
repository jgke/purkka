extern crate purkkaconverter;

use fragment::fragment::FragmentIterator;
use purkkaparser::parse_file;
use purkkaparser::parser::parse;
use purkkasyntax::*;
use purkkatoken::token::Token;
use purkkaconverter::transform;

fn test_parse_file(s: &str) -> S {
    println!("Testing following file:\n------\n{}\n------", s);
    parse_file("file.prk", s).0
}

fn test_convert_parse_file(s: &str) -> S {
    println!("Testing following file:\n------\n{}\n------", s);
    let (mut tree, op, ty) = parse_file("file.prk", s);
    transform(&mut tree, op, ty);
    tree
}


#[test]
fn parse_simple() {
    parse(
        &mut vec![
            Token::Let(0),
            Token::Identifier(1, From::from("foo")),
            Token::SemiColon(2),
        ]
        .iter()
        .peekable(),
        &vec![],
        &FragmentIterator::new("", ""),
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
            vec![Param::TypeOnly(Box::new(TypeSignature::Pointer {
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
    let fun = test_convert_parse_file(
        "fun main(argc: int, argv: [[char]]) -> int {
};",
    );
    let lambda = test_convert_parse_file(
        "const main = fun (argc: int, argv: [[char]]) -> int {
};",
    );
    assert_eq!(fun, lambda);
}

#[test]
fn parse_terse_fn() {
    let normal = test_convert_parse_file(
        "fun fn(arg: int) -> int {
    return arg;
};",
    );
    let terse = test_convert_parse_file("fun fn(arg: int) -> int => arg;");
    assert_eq!(normal, terse);
}

#[test]
fn parse_operators() {
    test_parse_file("operator left 5 &~ (left: long, bits: long) => left & ~bits;");
}

#[test]
fn parse_and_use_operator() {
    test_parse_file("operator left 5 +- (left: int, right: int) => left-right;let a = 1 +- 2;");
}

#[test]
fn parse_while() {
    test_parse_file("let a = while 1 { foo(); bar(); };");
    test_parse_file("let a = while 1 { foo(); bar(); } else { baz(); };");
}

#[test]
fn parse_multiple_statements() {
    test_parse_file("let a = if 1 { foo(); bar(); };");
}

#[test]
fn parse_assignment() {
    test_parse_file("let a = if 1 { let foo = 1; foo = 2; };");
}

#[test]
fn parse_complex_expressions() {
    test_parse_file("let a = 1 + (2 & 3++);");
}
