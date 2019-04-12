use kieliparser::parser::{parse, Token};

#[test]
fn parse_simple() {
    let tree = parse(&mut vec![Token::Let(), Token::Identifier(1), Token::SemiColon()].iter().peekable());
    dbg!(&tree);
}
