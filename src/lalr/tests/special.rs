#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Constant(i32),
}

fn is_special(token: &Token) -> bool {
    match token {
        Token::Constant(i) => i % 2 == 0,
        _ => panic!()
    }
}

lalr! {
    !Special -> is_special #Token::Constant;

    S  -> T;
    T -> B | A;
    A  -> #Token::Constant;
    B  -> #Special;
}

#[test]
fn parse_special() {
    println!("1");
    assert_eq!(
        driver(&mut [Token::Constant(1)].iter()),
        Some(S::T(S_T(T::A(T_A(A::Constant(A_Constant(Token::Constant(1))))))))
    );
    println!("2");
    assert_eq!(
        driver(&mut [Token::Constant(2)].iter()),
        Some(S::T(S_T(T::B(T_B(B::Special(B_Special(Token::Constant(2))))))))
    );
}
