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
    parse_file("let foo = 1 + 2;");
    parse_file("let foo = 1 + 2 * 3;");
    parse_file("let foo = 1 * 2 + 3;");
}

#[test]
fn parse_if_else() {
    parse_file("let foo = if 1 + 2 { something(); } elif 3 { something_else(); } else {};");
}
