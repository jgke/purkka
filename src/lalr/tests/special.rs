#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Constant(i32),
    Plus(),
}

pub struct State {
    special: i32
}

fn is_special(token: &Token, state: &State) -> bool {
    match token {
        Token::Constant(i) => state.special == *i,
        _ => panic!()
    }
}

fn make_special(state: &mut State, token: Token) {
    dbg!(&token);
    match token {
        Token::Constant(i) => state.special = i,
        _ => panic!()
    }
}

lalr! {
    !Special -> is_special #Token::Constant;

    S  -> T;
    T -> A #Token::Plus B;
    A  -> !make_special #Token::Constant;
    B  -> #Special;
}

#[test]
fn parse_special() {
    let mut state = State {
        special: 0,
    };
    assert_eq!(
        driver(&mut [Token::Constant(1), Token::Plus(), Token::Constant(1)].iter(), &mut state),
        Some(S::T(S_T(T::A(T_A(
                            A::Constant(A_Constant(Token::Constant(1))),
                            Token::Plus(),
                            B::Special(B_Special(Token::Constant(1))))))))
    );
}
