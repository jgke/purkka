use std::fmt;

use macrotoken::{MacroToken, MacroTokenType};
use tokentype;
use std::str::FromStr;

enum Number {
    Integer(i64),
    Float(f64),
}

enum Paren {
    OpenParen, CloseParen
}

enum ConstExprToken {
    Number(Number),
    Paren(Paren),
    Operator(tokentype::Operator)
}

use calculator::Number::*;
use calculator::Paren::*;
use calculator::ConstExprToken::*;

fn parse_number(num: &str) -> Number {
    if num.contains('.') {
        Float(f64::from_str(num).expect("Invalid float"))
    } else {
        Integer(i64::from_str(num).expect("Invalid int"))
    }
}

fn const_expr_token_from_macro(ty: &MacroTokenType) -> ConstExprToken {
    match ty {
        MacroTokenType::Identifier(_) => Number(Integer(0)),
        MacroTokenType::Number(num) => Number(parse_number(num)),
        MacroTokenType::StringLiteral(num) => panic!(),
        MacroTokenType::Operator(op) => Operator(*op),
        MacroTokenType::Punctuation(tokentype::Punctuation::OpenParen) => Paren(OpenParen),
        MacroTokenType::Punctuation(tokentype::Punctuation::CloseParen) => Paren(CloseParen),
        MacroTokenType::Punctuation(_) => panic!(),
        MacroTokenType::Other(_) => panic!(),
    }
}

fn eval_expression_recur(expr: &Vec<ConstExprToken>, index: &mut usize) -> Number {
    *index += 1;
    match expr.get(*index-1) {
        Some(Number(Integer(num))) => return Integer(*num),
        _ => panic!()
    }
}

pub fn eval_expression(expr: &Vec<MacroToken>) -> bool {
    let tokens: Vec<ConstExprToken> = expr.iter().map(|t| const_expr_token_from_macro(&t.ty)).collect();

    let result = eval_expression_recur(&tokens, &mut 0);
    match result {
        Integer(i) => i != 0,
        Float(f) => f != 0.0,
    }
}
