#![feature(box_patterns)]

extern crate purkkaconverter;

use fragment::fragment::FragmentIterator;
use purkkaconverter::transform;
use purkkaparser::parse_file;
use purkkaparser::parser::parse;
use purkkasyntax::*;
use purkkatoken::token::Token;

fn test_parse_file(s: &str) -> S {
    println!("Testing following file:\n------\n{}\n------", s);
    parse_file(
        "file.prk",
        s,
        &|_| panic!(),
        &|_, _| panic!(),
        &|_, _, _| panic!(),
    )
    .0
}

fn test_convert_parse_file(s: &str) -> S {
    println!("Testing following file:\n------\n{}\n------", s);
    let (mut tree, op, syms) = parse_file(
        "file.prk",
        s,
        &|_| panic!(),
        &|_, _| panic!(),
        &|_, _, _| panic!(),
    );
    transform(&mut tree, op, syms);
    tree
}

#[test]
fn parse_simple() {
    parse(
        From::from(vec![
            Token::Let(0),
            Token::Identifier(1, From::from("foo")),
            Token::SemiColon(2),
        ]),
        &vec![],
        &FragmentIterator::new("", ""),
        "",
        &|_| panic!(),
        &|_, _| panic!(),
        &|_, _, _| panic!(),
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
        .map(|Declaration::Declaration(_, ty, _)| &**ty)
}

#[test]
fn parse_types() {
    parse_ty("i32");
    parse_ty("i32 -> i32");
    parse_ty("(a: i32) -> i32");
    parse_ty("(a: i32, b: i32) -> i32");
    parse_ty("(a: i32, b: i32, c: i32) -> i32");
    parse_ty("(i32) -> i32");
    parse_ty("(i32, i32) -> i32");
    parse_ty("(i32, i32, i32) -> i32");
    parse_ty("(a: i32, i32, c: i32) -> i32");
    parse_ty("(a: (i32) -> i32) -> i32");
    parse_ty("((i32, i32)) -> i32");
    parse_ty("struct { foo: i32 }");
    parse_ty("struct { foo: i32 } -> i32");
    parse_ty("struct { foo: i32, bar: i32 }");
    parse_ty("struct { foo: i32, bar: struct { foo: i32 }, baz: i32 }");
    parse_ty("enum { foo }");
    parse_ty("enum { foo, bar, baz }");
    parse_ty("enum { foo(i32) }");
    parse_ty("enum { foo(i32, i32) }");
    parse_ty("enum { foo((i32, i32)) } -> (i32, i32)");
    parse_ty("[i32]");
    parse_ty("[i32] -> i32");
    parse_ty("[i32;5]");
    parse_ty("(foo: i32 -> i32) -> i32");
    parse_ty("(i32 -> i32) -> i32");
    parse_ty("i32 -> i32 -> i32");
    assert_eq!(
        get_ty(&parse_ty("&&i32 -> i32")).unwrap(),
        &TypeSignature::Function(
            vec![Param::TypeOnly(Box::new(TypeSignature::Pointer {
                nullable: false,
                ty: Box::new(TypeSignature::Pointer {
                    nullable: false,
                    ty: Box::new(TypeSignature::Primitive(Primitive::Int(32)))
                })
            }))],
            Box::new(TypeSignature::Primitive(Primitive::Int(32)))
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
        "fun main(argc: i32, argv: [[char]]) -> i32 {
        return 0;
};",
    );
    let lambda = test_convert_parse_file(
        "const main = fun (argc: i32, argv: [[char]]) -> i32 {
        return 0;
};",
    );
    assert_eq!(fun, lambda);
}

#[test]
fn parse_terse_fn() {
    let normal = test_convert_parse_file(
        "fun fn(arg: i32) -> i32 {
    return arg;
};",
    );
    let terse = test_convert_parse_file("fun fn(arg: i32) -> i32 => arg;");
    assert_eq!(normal, terse);
}

#[test]
fn parse_operators() {
    test_parse_file("operator left 5 &~ (left: i64, bits: i64) => left & ~bits;");
}

#[test]
fn parse_and_use_operator() {
    test_parse_file("operator left 5 +- (left: i32, right: i32) => left-right;let a = 1 +- 2;");
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

#[test]
fn parse_multiple_decls() {
    let decls = Some(&test_parse_file("let a = 1, b = 2;"))
        .translation_unit()
        .units()
        .and_then(|t| t.get(0))
        .declaration()
        .map(|Declaration::Declaration(_, _, decls)| {
            decls.clone()
        })
        .unwrap();
    assert_eq!(decls.len(), 2);
    assert_eq!(decls[0].0, From::from("a"));
    assert_eq!(decls[1].0, From::from("b"));

    if let Some(box Expression::Literal(Literal::Integer(v))) = decls[0].1 {
        assert_eq!(v, 1);
    } else {
        unreachable!();
    }
    if let Some(box Expression::Literal(Literal::Integer(v))) = decls[1].1 {
        assert_eq!(v, 2);
    } else {
        unreachable!();
    }
}
