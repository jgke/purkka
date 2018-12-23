#![feature(plugin)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

enum Token {
    Constant,
    Plus, Minus, Times, Divide,
    OpenParen, CloseParen,
    c, d
}

lalr! {
    S -> SS;
    SS -> C C;
    C -> #Token::c C | #Token::d;
}

//lalr! {
//    S -> PlusExpr;
//    PlusExpr -> TimesExpr
//              | Plus. PlusExpr #Token::Plus TimesExpr
//              | Minus. PlusExpr #Token::Minus TimesExpr;
//    TimesExpr -> PrimaryExpr
//               | Times. TimesExpr #Token::Times PrimaryExpr
//               | Divide. TimesExpr #Token::Divide PrimaryExpr;
//    PrimaryExpr -> #Token::Constant
//                 | #Token::OpenParen &PlusExpr #Token::CloseParen;
//}

#[test]
fn it_compiles() {
}
