#![feature(plugin)]
#![plugin(lalr)]
#![allow(dead_code)]

enum Token {
    Constant,
    Plus, Minus, Times, Divide,
    OpenParen, CloseParen
}

lalr! {
    S -> SS;
    SS -> C C;
    C -> #Token::Plus C | #Token::Minus;
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
