use macrotoken::{MacroToken, MacroTokenType};
use tokentype;
use tokentype::{Operator};
use std::str::FromStr;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Paren {
    OpenParen, CloseParen
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum ConstExprToken {
    Number(i64),
    Paren(Paren),
    Op(tokentype::Operator)
}

use calculator::Paren::*;
use calculator::ConstExprToken::*;

fn parse_number(num: &str) -> i64 {
    let mut num_len = 0;
    let mut iter = num.chars().peekable();
    loop {
        let c = iter.next();
        if c.is_some() {
            match c.unwrap() {
                '0' ..= '9' => num_len += 1,
                _ => break
            };
        } else {
            break;
        }
    }
    i64::from_str(&num[0..num_len]).expect("Invalid int")
}

fn const_expr_token_from_macro(ty: &MacroTokenType) -> ConstExprToken {
    match ty {
        MacroTokenType::Identifier(_) => Number(1),
        MacroTokenType::Number(num) => Number(parse_number(num)),
        MacroTokenType::StringLiteral(_) => panic!(),
        MacroTokenType::Operator(op) => Op(*op),
        MacroTokenType::Punctuation(tokentype::Punctuation::OpenParen) => Paren(OpenParen),
        MacroTokenType::Punctuation(tokentype::Punctuation::CloseParen) => Paren(CloseParen),
        MacroTokenType::Punctuation(_) => panic!("Invalid character in expression: {:?}", ty),
        MacroTokenType::Sizeof(_) => panic!("Cannot use sizeof expressions in #if expressions"),
        MacroTokenType::Other(_) => panic!("Invalid character in expression: {:?}", ty),
    }
}

// int -> bool
macro_rules! expr1b {
    ($stack:ident, $func:expr) => {{
        let right = $stack.pop().unwrap();
        // the left is a bogus number
        $stack.pop().unwrap();
        let result = if $func(right) { 1 } else { 0 };
        $stack.push(result);
    }}
}

// int -> int
macro_rules! expr1i {
    ($stack:ident, $func:expr) => {{
        let right = $stack.pop().unwrap();
        // the left is a bogus number
        $stack.pop().unwrap();
        let result = $func(right);
        $stack.push(result);
    }}
}

// (int, int) -> bool
macro_rules! expr2b {
    ($stack:ident, $func:expr) => {{
        let right = $stack.pop().unwrap();
        let left = $stack.pop().unwrap();
        let result = if $func(left, right) { 1 } else { 0 };
        $stack.push(result);
    }}
}

// (int, int) -> int
macro_rules! expr2i {
    ($stack:ident, $func:expr) => {{
        let right = $stack.pop().unwrap();
        let left = $stack.pop().unwrap();
        let result = $func(left, right);
        $stack.push(result);
    }}
}

fn eval_expression_postfix(expr: Vec<ConstExprToken>) -> i64 {
    let mut stack: Vec<i64> = Vec::new();
    let mut iter = expr.into_iter().peekable();
    while iter.peek().is_some() {
        let tok = iter.next().unwrap();
        match tok {
            Number(t) => stack.push(t),
            // unary
            Op(Operator::Not) => expr1b!(stack, |x| x != 0),
            Op(Operator::BitNot) => expr1i!(stack, |x: i64| !x),

            // bitwise
            Op(Operator::BitShiftLeft) => expr2i!(stack, |l, r| l << r),
            Op(Operator::BitShiftRight) => expr2i!(stack, |l, r| l >> r),
            Op(Operator::BitAnd) => expr2i!(stack, |l, r| l & r),
            Op(Operator::BitXor) => expr2i!(stack, |l, r| l ^ r),
            Op(Operator::BitOr) => expr2i!(stack, |l, r| l | r),

            // logical
            Op(Operator::And) => expr2b!(stack, |l, r| l != 0 && r != 0),
            Op(Operator::Or) => expr2b!(stack, |l, r| l != 0 || r != 0),
            Op(Operator::MoreThan) => expr2b!(stack, |l, r| l > r),
            Op(Operator::LessThan) => expr2b!(stack, |l, r| l < r),
            Op(Operator::MoreEqThan) => expr2b!(stack, |l, r| l >= r),
            Op(Operator::LessEqThan) => expr2b!(stack, |l, r| l <= r),
            Op(Operator::Equals) => expr2b!(stack, |l, r| l == r),

            // numerical
            Op(Operator::Plus) => expr2i!(stack, |l, r| l + r),
            Op(Operator::Minus) => expr2i!(stack, |l, r| l - r),
            Op(Operator::Mod) => expr2i!(stack, |l, r| l % r),
            Op(Operator::Times) => expr2i!(stack, |l, r| l * r),
            Op(Operator::Divide) => expr2i!(stack, |l, r| l / r),

            // ternary
            Op(Operator::Terniary) => panic!(),
            Op(Operator::TerniaryAlternative) => {
                assert_eq!(iter.next(), Some(Op(Operator::Terniary)));
                let right = stack.pop().unwrap();
                let left = stack.pop().unwrap();
                let condition = stack.pop().unwrap();
                if condition == 0 {
                    stack.push(left);
                } else {
                    stack.push(right);
                }
            },

            Op(op) => panic!("Unsupported operator in macro expression: {:?}", op),

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
                let op_prec = tokentype::get_precedence(&op);
                if op_prec == 1 {
                    // hack: make unary operators binary
                    out.push(Number(0));
                }
                loop {
                    let top = stack_top(&stack);
                    match top {
                        Some(Paren(OpenParen)) => break,
                        None => break,
                        Some(Op(stack_op)) => {
                            /*
        while ((there is an operator at the top of the operator stack with greater precedence)
               or (the operator at the top of the operator stack has equal precedence and is left associative))
              :
                             */
                            let stack_op_prec = tokentype::get_precedence(&stack_op);
                            if !(stack_op_prec < op_prec || (stack_op_prec == op_prec && tokentype::is_left_associative(&stack_op))) {
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
    assert!(expr.len() > 0);
    let tokens: Vec<ConstExprToken> = expr.iter().map(|t| const_expr_token_from_macro(&t.ty)).collect();
    let shunted = shunt(&tokens);
    let result = eval_expression_postfix(shunted);

    result != 0
}

#[test]
fn shunt_const() {
    assert_eq!(
        shunt(&vec![Number(0)]),
        vec![Number(0)]
        );
}

#[test]
fn shunt_plus() {
    assert_eq!(
        shunt(&vec![Number(0), Op(Operator::Plus), Number(1), Op(Operator::Plus), Number(2)]),
        vec![Number(0), Number(1), Op(Operator::Plus), Number(2), Op(Operator::Plus)],
        );
}

#[test]
fn shunt_ternary() {
    assert_eq!(
        shunt(&vec![Number(0), Op(Operator::Plus), Number(0),
            Op(Operator::Terniary), Number(1), Op(Operator::Plus), Number(2),
            Op(Operator::TerniaryAlternative), Number(3), Op(Operator::Minus), Number(4)]),
            vec![
            Number(0), Number(0), Op(Operator::Plus),
            Number(1), Number(2), Op(Operator::Plus),
            Number(3), Number(4), Op(Operator::Minus),
            Op(Operator::TerniaryAlternative), Op(Operator::Terniary)
            ],
        );
}


#[test]
fn eval_ternary() {
    assert_eq!(
        eval_expression_postfix(shunt(&vec![Number(0), Op(Operator::Plus), Number(1),
            Op(Operator::Terniary), Number(1), Op(Operator::Plus), Number(2),
            Op(Operator::TerniaryAlternative), Number(3), Op(Operator::Minus), Number(4)])),
            -1
        );
}

// testing utils

#[cfg(test)]
use tokentype::OPERATORS;

#[cfg(test)]
fn get_op(op: &str) -> Option<Operator> {
    for (s, operator) in OPERATORS {
        if s == &op {
            return Some(**operator)
        }
    }
    None
}

#[cfg(test)]
fn get_paren(paren: &str) -> Option<Paren> {
    match paren {
        "(" => Some(OpenParen),
        ")" => Some(CloseParen),
        _ => None
    }
}

#[cfg(test)]
fn to_token(s: &str) -> ConstExprToken {
    get_op(s).map(|t| Op(t))
        .or_else(|| get_paren(s).map(|t| Paren(t)))
        .unwrap_or_else(|| Number(parse_number(s)))
}

#[cfg(test)]
fn check(expr: &str, expected: i64) {
    let vec: Vec<ConstExprToken> = expr.split(' ').map(to_token).collect();
    assert_eq!(eval_expression_postfix(shunt(&vec)), expected);
}

#[test]
fn common_operations() {
    check("1 + 2 + 3", 6);
    check("1 + 2 - 3", 0);
    check("1 - 3 + 2", 0);
    check("1 * 3 + 2", 5);
    check("1 + 3 * 2", 7);
    check("6 / 3 + 2", 4);
    check("6 / 4 + 2", 3);
    check("0 ? 1 : 2", 1);
    check("1 ? 1 : 2", 2);
}
