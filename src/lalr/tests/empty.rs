#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

type State = ();

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Constant(),
    Plus(),
    Minus(),
}

lalr! {
    S  -> Ss;
    Ss -> A | C;
    A  -> #Token::Constant
        | Epsilon;
    C  -> #Token::Minus Cc;
    Cc -> #Token::Minus Cc
        | Epsilon;
}

#[test]
fn parse_empty_1() {
    assert_eq!(
        driver(&mut [Token::Constant()].iter(), &mut ()),
        Ok(S::Ss(S_Ss(Ss::A(Ss_A(A::Constant(A_Constant(Token::Constant())))))))
    );
}

#[test]
fn parse_empty_2() {
    assert_eq!(
        driver(&mut [Token::Constant()].iter(), &mut ()),
        Ok(S::Ss(S_Ss(Ss::A(Ss_A(A::Constant(A_Constant(Token::Constant())))))))
    );
}

#[test]
fn parse_empty_3() {
    assert_eq!(
        driver(&mut [].iter(), &mut ()),
        Ok(S::Ss(S_Ss(Ss::A(Ss_A(A::Epsilon(A_Epsilon()))))))
    );
}

#[test]
fn parse_empty_4() {
    assert_eq!(
        driver(&mut [Token::Minus(), Token::Minus()].iter(), &mut ()),
        Ok(S::Ss(S_Ss(Ss::C(Ss_C(
            C::Minus(C_Minus(
                Token::Minus(),
                Cc::Minus(Cc_Minus(
                    Token::Minus(),
                    Box::new(Cc::Epsilon(Cc_Epsilon()))
        )))))))))
    );
}
