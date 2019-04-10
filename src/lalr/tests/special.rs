#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Constant(i32),
    Plus(),
    Times(),
}

pub struct State {
    special: i32
}

fn is_special(state: &State, token: &Token) -> bool {
    match token {
        Token::Constant(i) => state.special == *i && *i <= 10,
        _ => panic!()
    }
}

fn is_extra_special(state: &State, token: &Token) -> bool {
    match token {
        Token::Constant(i) => state.special == *i && *i > 10,
        _ => panic!()
    }
}

fn make_special(state: &mut State, token: Token) {
    match token {
        Token::Constant(i) => state.special = i,
        _ => panic!()
    }
}

lalr! {
    !Special -> is_special #Token::Constant;
    !ExtraSpecial -> is_extra_special #Token::Constant;

    S -> T;
    T -> A #Token::Plus B
       | C. A #Token::Times C;
    A -> !make_special #Token::Constant;
    B -> #Special;
    C -> #ExtraSpecial;
}

#[test]
fn parse_special() {
    let mut state = State {
        special: 0,
    };
    assert_eq!(
        driver(&mut [Token::Constant(1), Token::Plus(), Token::Constant(1)].iter(), &mut state),
        Ok(S::T(T::A(
                    A::Constant(Token::Constant(1)),
                    Token::Plus(),
                    B::Special(Token::Constant(1)))))
    );
}

#[test]
fn parse_extra_special() {
    let mut state = State {
        special: 0,
    };
    assert_eq!(
        driver(&mut [Token::Constant(11), Token::Times(), Token::Constant(11)].iter(), &mut state),
        Ok(S::T(T::C(
                    A::Constant(Token::Constant(11)),
                    Token::Times(),
                    C::ExtraSpecial(Token::Constant(11)))))
    );
}
