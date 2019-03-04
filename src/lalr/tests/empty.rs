#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

extern crate lalr_runtime;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Constant(),
}

lalr! {
    S -> A;
    A -> #Token::Constant
       | Epsilon;
}

#[test]
fn parse_empty() {
    let tree_2 = driver(&mut [Token::Constant()].iter());
    assert_eq!(
        tree_2,
        Some(S::A(S_A(A::Constant(A_Constant(Token::Constant())))))
    );
    let tree = driver(&mut [].iter());
    assert_eq!(
        tree,
        None
    );
}
