use kieliparser::parse_file;
use kieliparser::parser::parse;
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

#[test]
fn parse_types() {
    parse_file("let foo: int;");
    parse_file("let foo: int -> int;");
    parse_file("let foo: (a: int) -> int;");
    parse_file("let foo: (a: int, b: int) -> int;");
    parse_file("let foo: (a: int, b: int, c: int) -> int;");
    parse_file("let foo: (int) -> int;");
    parse_file("let foo: (int, int) -> int;");
    parse_file("let foo: (int, int, int) -> int;");
    parse_file("let foo: (a: int, int, c: int) -> int;");
    parse_file("let foo: (a: (int) -> int) -> int;");
    parse_file("let foo: ((int, int)) -> int;");
    parse_file("let foo: struct { foo: int };");
    parse_file("let foo: struct { foo: int } -> int;");
    parse_file("let foo: struct { foo: int, bar: int };");
    parse_file("let foo: struct { foo: int, bar: struct { foo: int }, baz: int };");
    parse_file("let foo: enum { foo };");
    parse_file("let foo: enum { foo, bar, baz };");
    parse_file("let foo: enum { foo(int) };");
    parse_file("let foo: enum { foo(int, int) };");
    parse_file("let foo: enum { foo((int, int)) };");
}
