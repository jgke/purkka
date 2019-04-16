use std::iter::Peekable;
use std::collections::HashMap;
use std::rc::Rc;

use crate::token::Token;

macro_rules! maybe_read_token {
    ($iter:expr, $tok:path) => {
        if let Some($tok(..)) = $iter.peek() {
            $iter.next();
            true
        } else {
            false
        }
    }
}

macro_rules! unexpected_token {
    ($token:expr, $iter:expr) => {
        match $token {
            None => panic!("Unexpected end of file"),
            Some(t) => panic!("Unexpected token: {:?}", t),
        }
    }
}

macro_rules! read_token {
    ($iter:expr, $tok:path) => {
        if let Some($tok(..)) = $iter.peek() {
            $iter.next().unwrap().clone()
        } else {
            unexpected_token!(&$iter.next(), $iter)
        }
    }
}

grammar! {
    S -> TranslationUnit;

    TranslationUnit
       -> Leaf. Unit #Token::SemiColon
        | List. Unit #Token::SemiColon TranslationUnit
        ;

    Unit
       -> Declaration
        | IncludeFile
        ;

    Declaration
       -> Declaration. Visibility #Token::Let Mutability #Token::Identifier MaybeType
        | Definition. Visibility #Token::Let Mutability #Token::Identifier MaybeType #Token::Assign AssignmentExpression
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Declaration { Declaration(bool, bool, Rc<str>, Option<Type>, Option<AssignmentExpression>) }
        ;

    MaybeType
       -> Type. #Token::Colon Type
        | NoType. Epsilon
        ;

    Type
       -> #Token::Identifier
        ;

    MaybeAssign
       -> #Token::Assign AssignmentExpression
        | Epsilon
        ;

    Mutability
       -> Mutable. #Token::Let
        | Const. #Token::Const
        ;

    Visibility
       -> Public. #Token::Pub
        | Private. Epsilon
        ;

    IncludeFile
       -> Normal. #Token::Include Path
        | C. #Token::IncludeC Path
        ;

    PrimaryExpression
       -> #Token::Identifier
        | #Token::Integer
        | #Token::Float
        ;

    Expression
       -> PrimaryExpression
        | Op. #Token::Operator ExprList
        | Prefix. #Token::Operator Expression
        ;

    ExprList -> Expression | Expression ExprList
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum ExprList { List(Vec<Box<Expression>>) }
        ;

    AssignmentExpression
       -> Expression
       ;

    Path -> #Token::Identifier;
}

pub type Iter<'a, 'b> = &'a mut Peekable<std::slice::Iter<'b, Token>>;
type PrecedenceMap = HashMap<Rc<str>, Precedence>;

fn default_bin_ops() -> PrecedenceMap {
    let mut precedence = HashMap::new();
    precedence.insert(From::from("*"), Precedence::binop(1));
    precedence.insert(From::from("+"), Precedence::binop(2));
    precedence.insert(From::from("-"), Precedence::binop(2));
    precedence
}

fn default_unary_ops() -> PrecedenceMap {
    let mut precedence = HashMap::new();
    precedence.insert(From::from("~"), Precedence::unary());
    precedence.insert(From::from("-"), Precedence::unary());
    precedence
}

pub fn parse(iter: Iter) -> S {
    let mut context = ParseContext {
        unary_precedence: default_unary_ops(),
        precedence: default_bin_ops(),
        iter
    };
    S::TranslationUnit(context.parse_translation_unit())
}

#[derive(Debug, Clone, Copy)]
struct Precedence {
    precedence: usize,
    param_count: usize,
    left_associative: bool,
}

impl Precedence {
    fn binop(precedence: usize) -> Precedence {
        Precedence { precedence, param_count: 2, left_associative: true }
    }
    fn unary() -> Precedence {
        Precedence { precedence: 1, param_count: 2, left_associative: true }
    }
}

struct ParseContext<'a, 'b> {
    unary_precedence: PrecedenceMap,
    precedence: PrecedenceMap,
    iter: Iter<'a, 'b>
}

impl ParseContext<'_, '_> {
    fn parse_translation_unit(&mut self) -> TranslationUnit {
        let unit = self.parse_unit();
        let sc = read_token!(self.iter, Token::SemiColon);
        let has_more = self.iter.peek().is_some();
        match has_more {
            true => TranslationUnit::List(unit, sc, Box::new(self.parse_translation_unit())),
            false => TranslationUnit::Leaf(unit, sc)
        }
    }

    fn parse_unit(&mut self) -> Unit {
        match_first!(
            self.iter.peek() => _t,
            default unexpected_token!(_t, self.iter),

            Declaration => Unit::Declaration(self.parse_declaration()),
            IncludeFile => Unit::IncludeFile(self.parse_include()),
            )
    }

    fn parse_declaration(&mut self) -> Declaration {
        let visible = maybe_read_token!(self.iter, Token::Pub);
        let mutable = match self.iter.next() {
            Some(Token::Let()) => true,
            Some(Token::Const()) => false,
            t => unexpected_token!(t, self.ter)
        };
        let ident = match self.iter.next() {
            Some(Token::Identifier(s)) => s,
            t => unexpected_token!(t, self.ter)
        };
        let ty = match_first!(
            self.iter.peek() => _t,
            default None,

            MaybeType => Some(self.parse_type()),
            );

        match self.iter.peek() {
            Some(Token::Operator(t)) if &**t == "=" => {
                self.iter.next();
                let expr = self.parse_assignment_expr();
                Declaration::Declaration(visible, mutable, ident.clone(), ty, Some(expr))
            }
            _ => Declaration::Declaration(visible, mutable, ident.clone(), ty, None)
        }
    }

    fn parse_type(&mut self) -> Type {
        unexpected_token!(self.iter.peek(), self.iter)
    }

    fn parse_include(&mut self) -> IncludeFile {
        unexpected_token!(self.iter.peek(), self.iter)
    }

    fn parse_assignment_expr(&mut self) -> AssignmentExpression {
        AssignmentExpression::Expression(self.parse_expression(12))
    }

    fn parse_expression(&mut self, precedence: usize) -> Expression {
        match self.iter.peek() {
            Some(Token::Operator(p)) => match self.unary_precedence.get(p) {
                Some(n) if n.left_associative && n.param_count == 1 => {
                    assert_eq!(n.left_associative, true);
                    let op = self.iter.next().unwrap().clone();
                    let expr = self.parse_expression(n.precedence);
                    return Expression::Op(op, ExprList::List(vec![Box::new(expr)]));
                }
                _ => panic!("Unexpected operator: {:?}", p)
            }
            _ => {}
        }
        let mut expr = Expression::PrimaryExpression(self.parse_primary_expression());
        loop {
            match self.iter.peek() {
                Some(Token::Operator(p)) => match self.precedence.get(p).map(|t| *t) {
                    Some(n) if precedence >= n.precedence => {
                        let mut left = vec![Box::new(expr)];
                        let op = self.iter.next().unwrap().clone();
                        let mut tail = (1..n.param_count).into_iter()
                            .map(|_| self.parse_expression(n.precedence))
                            .map(Box::new)
                            .collect();
                        left.append(&mut tail);
                        expr = Expression::Op(op, ExprList::List(left));
                    }
                    Some(_) => break,
                    None => panic!("Unknown operator: {}", p)
                }
                _ => break
            };
        }
        expr
    }
    fn parse_primary_expression(&mut self) -> PrimaryExpression {
        match self.iter.next() {
            Some(t@Token::Identifier(..)) => PrimaryExpression::Identifier(t.clone()),
            Some(t@Token::Integer(..)) => PrimaryExpression::Integer(t.clone()),
            Some(t@Token::Float(..)) => PrimaryExpression::Float(t.clone()),
            t => unexpected_token!(t, self.iter)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn to_token(prec: &PrecedenceMap, s: &str) -> Token {
        prec.get(s)
            .map(|_| Token::Operator(From::from(s)))
            .unwrap_or_else(|| Token::Integer(s.parse().unwrap()))
    }

    fn eval_tree(expr: &Expression) -> i128 {
        match expr {
            Expression::PrimaryExpression(PrimaryExpression::Integer(Token::Integer(e))) => *e,
            Expression::PrimaryExpression(_) => unreachable!(),
            Expression::Op(op, ExprList::List(list)) => {
                let op_s: &str = if let Token::Operator(s) = op { &*s } else { unreachable!() };
                match op_s {
                    "+" => eval_tree(&*list[0]) + eval_tree(&*list[1]),
                    "-" => eval_tree(&*list[0]) - eval_tree(&*list[1]),
                    "*" => eval_tree(&*list[0]) * eval_tree(&*list[1]),
                    _ => unreachable!()
                }
            }
            Expression::Prefix(op, expr) => {
                let op_s: &str = if let Token::Operator(s) = op { &*s } else { unreachable!() };
                match op_s {
                    "~" => !eval_tree(&*expr),
                    _ => unreachable!()
                }
            }
        }
    }

    fn check(expr: &str, expected: i128) {
        let unary_precedence = default_unary_ops();
        let precedence = default_bin_ops();
        let both = unary_precedence.clone().into_iter().chain(precedence.clone().into_iter()).collect();
        let vec: Vec<Token> = expr.split(' ').map(|t| to_token(&both, t)).collect();
        let mut context = ParseContext { precedence, unary_precedence, iter: &mut vec.iter().peekable() };
        let result = eval_tree(&context.parse_expression(12));
        println!("{} = {} (expected: {})", expr, result, expected);
        assert_eq!(result, expected);
    }

    macro_rules! check {
        ($expr:expr) => {
            check(stringify!($expr), $expr)
        }
    }

    #[test]
    fn parse_expr_and_eval() {
        check!(1 + 1);
        check!(1 + -1);
        check!(1 * -1);
        check!(1 + 2 * 2);
        check!(1 - 2 * 2);
    }
}
