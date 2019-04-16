use kieliparser::parse_file;
use kieliparser::parser::parse;
use kieliparser::token::Token;

#[test]
fn parse_simple() {
    parse(&mut vec![
          Token::Let(),
          Token::Identifier(From::from("foo")),
          Token::SemiColon()].iter().peekable());
}

#[test]
fn parse_simple_str() {
    parse_file("let foo = 1;");
    dbg!(parse_file("let foo = 1 + 2;"));
    dbg!(parse_file("let foo = 1 + 2 * 3;"));
    dbg!(parse_file("let foo = 1 * 2 + 3;"));
}
