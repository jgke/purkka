#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

type State = ();

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Constant(),
    Plus(),
    Times(),
}

lalr! {
    S -> Expr;
    Expr -> PrimaryExpr
        | 1 l: Plus. Expr #Token::Plus Expr
        | 2 l: Times. Expr #Token::Times Expr;
    PrimaryExpr -> #Token::Constant;
}

#[test]
fn parse_priority() {
    use Token::*;

    assert_eq!(
        driver(&mut [Constant(), Times(), Constant(), Plus(), Constant()].iter(), &mut ()),
        Ok(S::Expr(S_Expr(
                    Expr::Plus(Expr_Plus(
                            Box::new(Expr::Times(Expr_Times(
                                    Box::new(Expr::PrimaryExpr(Expr_PrimaryExpr(
                                            PrimaryExpr::Constant(PrimaryExpr_Constant(
                                                    Constant()))))),
                                    Times(),
                                    Box::new(Expr::PrimaryExpr(Expr_PrimaryExpr(
                                            PrimaryExpr::Constant(PrimaryExpr_Constant(
                                                    Constant())))))))),
                            Plus(),
                            Box::new(Expr::PrimaryExpr(Expr_PrimaryExpr(
                                    PrimaryExpr::Constant(PrimaryExpr_Constant(
                                            Constant()))))))))))
    );
    assert_eq!(
        driver(&mut [Constant(), Plus(), Constant(), Times(), Constant()].iter(), &mut ()),
        Ok(S::Expr(S_Expr(
                    Expr::Plus(Expr_Plus(
                            Box::new(Expr::PrimaryExpr(Expr_PrimaryExpr(
                                    PrimaryExpr::Constant(PrimaryExpr_Constant(
                                            Constant()))))),
                            Plus(),
                            Box::new(Expr::Times(Expr_Times(
                                    Box::new(Expr::PrimaryExpr(Expr_PrimaryExpr(
                                            PrimaryExpr::Constant(PrimaryExpr_Constant(
                                                    Constant()))))),
                                    Times(),
                                    Box::new(Expr::PrimaryExpr(Expr_PrimaryExpr(
                                            PrimaryExpr::Constant(PrimaryExpr_Constant(
                                                    Constant())))))))))))))
    );
}
