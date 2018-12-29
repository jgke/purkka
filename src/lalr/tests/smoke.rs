#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

extern crate lalr_runtime;

use std::fmt;

#[derive(Clone,Debug,PartialEq)]
enum Token {
    Constant(),
    Plus(),
    Minus(),
    Times(),
    Divide(),
    OpenParen(),
    CloseParen(),
}


lalr! {
    S -> PlusExpr;
    PlusExpr -> TimesExpr
              | Plus. PlusExpr #Token::Plus TimesExpr
              | Minus. PlusExpr #Token::Minus TimesExpr;
    TimesExpr -> PrimaryExpr
               | Times. TimesExpr #Token::Times PrimaryExpr
               | Divide. TimesExpr #Token::Divide PrimaryExpr;
    PrimaryExpr -> #Token::Constant
                 | #Token::OpenParen &PlusExpr #Token::CloseParen;
}

#[test]
fn it_compiles() {}

#[test]
fn token_parsing() {
    use Token::*;

    let tree = driver(&mut [Constant(), Plus(), OpenParen(), Constant(), CloseParen()].iter());
    assert_eq!(
        tree,
        Some(S::PlusExpr(S_PlusExpr(
                    PlusExpr::Plus(PlusExpr_Plus(
                            Box::new(PlusExpr::TimesExpr(PlusExpr_TimesExpr(
                                        TimesExpr::PrimaryExpr(TimesExpr_PrimaryExpr(
                                                PrimaryExpr::Constant(PrimaryExpr_Constant(
                                                        Constant()))))))),
                            Plus(),
                            TimesExpr::PrimaryExpr(TimesExpr_PrimaryExpr(
                                    PrimaryExpr::OpenParen(PrimaryExpr_OpenParen(
                                            OpenParen(),
                                            Box::new(PlusExpr::TimesExpr(PlusExpr_TimesExpr(
                                                        TimesExpr::PrimaryExpr(TimesExpr_PrimaryExpr(
                                                                PrimaryExpr::Constant(PrimaryExpr_Constant(
                                                                        Constant()))))))),
                                            CloseParen())))))))))
        );
}
