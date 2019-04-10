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
//lalr! {
//    S -> A;
//    A -> C C;
//    C -> #Token::Times C | #Token::Plus;
//}

#[test]
fn it_compiles() {}

#[test]
fn parse_simple() {
    use Token::*;

    //let tree = driver(&mut [Times(), Times(), Plus(), Plus()].iter());
    let tree = driver(&mut [Constant(), Plus(), OpenParen(), Constant(), CloseParen()].iter(), &mut ());
    assert_eq!(
        tree,
        Ok(S::PlusExpr(PlusExpr::Plus(
            Box::new(PlusExpr::TimesExpr(
                TimesExpr::PrimaryExpr(PrimaryExpr::Constant(
                    Constant()
                )))
            ),
            Plus(),
            TimesExpr::PrimaryExpr(PrimaryExpr::OpenParen(
                    OpenParen(),
                    Box::new(PlusExpr::TimesExpr(
                        TimesExpr::PrimaryExpr(PrimaryExpr::Constant(
                            Constant()
                        ))
                    )),
                    CloseParen()
                )
            )))
        )
    );
}
