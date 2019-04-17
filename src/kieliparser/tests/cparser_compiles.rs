use kieliparser::parse_file;
use kieliparser::parser::Maybe::*;
use kieliparser::parser::*;
use kieliparser::token::Token;

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
    parse_file("let foo = 1;");
    parse_file("let foo = 1 + 2;");
    parse_file("let foo = 1 + 2 * 3;");
    parse_file("let foo = 1 * 2 + 3;");
}

#[test]
fn parse_if_else() {
    parse_file("let foo = if 1 + 2 { something(); } elif 3 { something_else(); } else {};");
}

fn parse_ty(s: &str) -> S {
    parse_file(&format!("let foo: {};", s))
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
    parse_ty("(foo: int -> int) -> int");
    parse_ty("(int -> int) -> int");
    parse_ty("int -> int -> int");
    assert_eq!(
        Just(&parse_ty("&&int -> int"))
            .translation_unit()
            .leaf()
            .declaration()
            .ty()
            .from_just(),
        &TypeSignature::Function(
            vec![Param::Anon(Box::new(TypeSignature::Pointer {
                nullable: false,
                ty: Box::new(TypeSignature::Pointer {
                    nullable: false,
                    ty: Box::new(TypeSignature::Plain(From::from("int")))
                }
            )}))],
            Box::new(TypeSignature::Plain(From::from("int")))
        )
    );
}
