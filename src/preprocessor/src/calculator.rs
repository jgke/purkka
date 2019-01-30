use std::fmt;

use macrotoken::{MacroToken, MacroTokenType};
use tokentype;
use tokentype::Operator;
use std::str::FromStr;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Number {
    Integer(i64),
    Float(f64),
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Paren {
    OpenParen, CloseParen
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum ConstExprToken {
    Number(Number),
    Paren(Paren),
    Op(tokentype::Operator)
}

use calculator::Number::*;
use calculator::Paren::*;
use calculator::ConstExprToken::*;

static TRUTHY: ConstExprToken = Number(Integer(1));
static FALSY: ConstExprToken = Number(Integer(0));

fn parse_number(num: &str) -> Number {
    if num.contains('.') {
        Float(f64::from_str(num).expect("Invalid float"))
    } else {
        Integer(i64::from_str(num).expect("Invalid int"))
    }
}

fn const_expr_token_from_macro(ty: &MacroTokenType) -> ConstExprToken {
    match ty {
        MacroTokenType::Identifier(_) => TRUTHY,
        MacroTokenType::Number(num) => Number(parse_number(num)),
        MacroTokenType::StringLiteral(num) => panic!(),
        MacroTokenType::Operator(op) => Op(*op),
        MacroTokenType::Punctuation(tokentype::Punctuation::OpenParen) => Paren(OpenParen),
        MacroTokenType::Punctuation(tokentype::Punctuation::CloseParen) => Paren(CloseParen),
        MacroTokenType::Punctuation(_) => panic!(),
        MacroTokenType::Other(_) => panic!(),
    }
}

fn op_one_id(operand: Number,
             integer: impl Fn(i64) -> i64,
             float: impl Fn(f64) -> f64) -> Number {
    match operand {
        Integer(i) => Integer(integer(i)),
        Float(i) => Float(float(i)),
    }
}

fn op_one_bool(operand: Number,
               integer: impl Fn(i64) -> bool,
               float: impl Fn(f64) -> bool) -> Number {
    match operand {
        Integer(i) => Integer(if integer(i) { 1 } else {0}),
        Float(i) => Integer(if float(i) { 1 } else {0}),
    }
}

fn op_two_bool(left: Number, right: Number,
               integer: impl Fn(i64, i64) -> bool,
               float: impl Fn(f64, f64) -> bool) -> Number {
    match (left, right) {
        (Integer(l), Integer(r)) => Integer(if integer(l, r) { 1 } else {0}),
        (Integer(l), Float(r)) => Integer(if float(l as f64, r) { 1 } else {0}),
        (Float(l), Integer(r)) => Integer(if float(l, r as f64) { 1 } else {0}),
        (Float(l), Float(r)) => Integer(if float(l, r) { 1 } else {0}),
    }
}

macro_rules! expr1b {
    ($stack:ident, $int:expr, $float:expr) => {{
        let result = op_one_bool($stack.pop().unwrap(), $int, $float);
        $stack.push(result);
    }}
}

macro_rules! expr2b {
    ($stack:ident, $int:expr, $float:expr) => {{
        let right = $stack.pop().unwrap();
        let left = $stack.pop().unwrap();
        let result = op_two_bool(left, right, $int, $float);
        $stack.push(result);
    }}
}

fn eval_expression_postfix(expr: Vec<ConstExprToken>, index: &mut usize) -> Number {
    let mut stack: Vec<Number> = Vec::new();
    for tok in expr {
        match tok {
            Number(t) => stack.push(t),
            // unary
            Op(Operator::Not) => expr1b!(stack, |x| x != 0, |x| x != 0.0),
            Op(Operator::BitNot) => unimplemented!(),

            // bitwise
            Op(Operator::BitShiftLeft) => unimplemented!(),
            Op(Operator::BitShiftRight) => unimplemented!(),
            Op(Operator::BitAnd) => unimplemented!(),
            Op(Operator::BitXor) => unimplemented!(),
            Op(Operator::BitOr) => unimplemented!(),

            // logical
            Op(Operator::And) => unimplemented!(),
            Op(Operator::Or) => unimplemented!(),
            Op(Operator::MoreThan) => expr2b!(stack, |l, r| l > r, |l, r| l > r),
            Op(Operator::LessThan) => unimplemented!(),
            Op(Operator::MoreEqThan) => unimplemented!(),
            Op(Operator::LessEqThan) => unimplemented!(),

            // numerical
            Op(Operator::Plus) => unimplemented!(),
            Op(Operator::Minus) => unimplemented!(),
            Op(Operator::Mod) => unimplemented!(),
            Op(Operator::Times) => unimplemented!(),
            Op(Operator::Divide) => unimplemented!(),

            // ternary
            Op(Operator::Terniary) => unimplemented!(),
            Op(Operator::TerniaryAlternative) => unimplemented!(),

            Op(_) => panic!(),

            Paren(_) => unreachable!()
        }
    }
    assert_eq!(stack.len(), 1);
    stack[0]
}

fn stack_top(stack: &Vec<ConstExprToken>) -> Option<ConstExprToken> {
    if stack.len() == 0 {
        None
    } else {
        Some(stack[stack.len()-1])
    }
}


// Use the shunting yard algorithm to convert infix to postfix
fn shunt(expr: &Vec<ConstExprToken>) -> Vec<ConstExprToken> {
    let mut stack = Vec::new();
    let mut out = Vec::new();
    let mut i = 0;

    while i < expr.len() {
        let tok = expr[i];
        match tok {
            Number(_) => out.push(expr[i].clone()),
            Op(op) => {
                loop {
                    let top = stack_top(&stack);
                    match top {
                        Some(Paren(OpenParen)) => break,
                        None => break,
                        Some(Op(stack_op)) => {
                            let op_prec = tokentype::get_precedence(&op);
                            let stack_op_prec = tokentype::get_precedence(&stack_op);
                            if stack_op_prec.0 < op_prec.0 && !stack_op_prec.1 {
                                break;
                            }
                        }

                        Some(Number(_)) => unreachable!(),
                        Some(Paren(CloseParen)) => unreachable!(),
                    }
                    out.push(stack.pop().unwrap());
                }
                stack.push(tok);
            }
            Paren(OpenParen) => stack.push(tok),
            Paren(CloseParen) => {
                loop {
                    let top = stack_top(&stack);
                    match top {
                        Some(Paren(OpenParen)) => {
                            stack.pop();
                            break;
                        }
                        Some(_) => out.push(stack.pop().unwrap()), 
                        None => panic!()
                    }
                }
            }
        }
        i += 1;
    }
    if stack.iter().any(|t| t == &Paren(OpenParen) || t == &Paren(CloseParen)) {
        panic!();
    }
    out.append(&mut stack.drain(..).rev().collect());
    out
}

pub fn eval_expression(expr: &Vec<MacroToken>) -> bool {
    let tokens: Vec<ConstExprToken> = expr.iter().map(|t| const_expr_token_from_macro(&t.ty)).collect();

    let result = eval_expression_postfix(shunt(&tokens), &mut 0);
    match result {
        Integer(i) => i != 0,
        Float(f) => f != 0.0,
    }
}


