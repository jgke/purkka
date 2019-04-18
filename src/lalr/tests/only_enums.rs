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

#[test]
fn match_first() {
    let vec = vec![Token::Plus()];
    let iter = &mut vec.iter().peekable();
    let res = match_first!(
        iter.peek() => _t,
        default None,

        B => Some(1),
        C => Some(2),
    );
    assert_eq!(res, Some(1));
}

#[test]
fn match_first_2() {
    let vec = vec![Token::Minus()];
    let iter = &mut vec.iter().peekable();
    let res = match_first!(
        iter.peek() => _t,
        default None,

        B => Some(1),
        C => Some(2),
    );
    assert_eq!(res, Some(2));
}
