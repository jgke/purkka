#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Constant(),
    Plus(),
}

lalr! {
    S  -> A | B;
    A  -> #Token::Constant
        | Epsilon;
    B  -> #Token::Plus Bb;
    Bb -> #Token::Plus Bb
        | Epsilon;
}

#[test]
fn parse_empty() {
    println!("1");
    assert_eq!(
        driver(&mut [Token::Constant()].iter()),
        Some(S::A(S_A(A::Constant(A_Constant(Token::Constant())))))
    );
    println!("2");
    assert_eq!(
        driver(&mut [Token::Constant()].iter()),
        Some(S::A(S_A(A::Constant(A_Constant(Token::Constant())))))
    );
    println!("3");
    assert_eq!(
        driver(&mut [].iter()),
        None
    );
}
