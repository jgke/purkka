#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Plus(),
    Minus(),
}

grammar! {
    S -> A;
    A -> B | C;
    B -> #Token::Plus;
    C -> #Token::Minus;
}

#[test]
fn grammar_compiles() {
    use Token::*;

    let _tree = S::A(A::B(B::Plus(Plus())));
}
