use std::iter::Peekable;
use std::collections::HashMap;
use std::rc::Rc;

use crate::token::Token;

// 'Re-implement' Option<> because
//  a) error[E0116]: cannot define inherent `impl` for a type outside of the crate where the type is defined
//  b) I don't want to create a new trait for every single AST node (although this might be a good
//      idea in the future)
//  c) This gives some way for future
enum Maybe<T> {
    Just(T),
    Nothing
}

use Maybe::*;

impl<T> Maybe<T> {
    fn from_just(self) -> T {
        if let Just(t) = self { t } else { panic!() }
    }
}

impl<T> From<Option<T>> for Maybe<T> {
    fn from(that: Option<T>) -> Maybe<T> {
        match that {
            Some(t) => Just(t),
            None => Nothing
        }
    }
}

impl<T> Into<Option<T>> for Maybe<T> {
    fn into(self) -> Option<T> {
        match self {
            Just(t) => Some(t),
            Nothing => None
        }
    }
}

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

macro_rules! impl_enter {
    (@implpat $this:ident, $var:ident, $t:ident, 1) => { $this::$var($t, ..) };
    (@implpat $this:ident, $var:ident, $t:ident, 3) => { $this::$var(_, _, $t, ..) };
    (@implpat $this:ident, $var:ident, $t:ident, 5) => { $this::$var(_, _, _, _, $t, ..) };
    (@implpat $this:ident, $var:ident, $t:ident, 6) => { $this::$var(_, _, _, _, _, $t, ..) };
    (@iflet $self:ident, $this:ident, $variant_name:ident, $count:tt) => {
        if let Just(impl_enter!(@implpat $this, $variant_name, t, $count)) = $self {
            Just(t)
        } else {
            Nothing
        }
    };
    (@iflet @fmap $self:ident, $this:ident, $variant_name:ident, $count:tt) => {
        if let Just(impl_enter!(@implpat $this, $variant_name, t, $count)) = $self {
            From::from(t.as_ref())
        } else {
            Nothing
        }
    };
    (fmap, $this:ident, $variant_name:ident, $that:ty, $fn_name:ident, $($pat:tt)*) => {
        impl Maybe<&$this> {
            fn $fn_name(&self) -> Maybe<&$that> {
                impl_enter!(@iflet @fmap self, $this, $variant_name, $($pat)*)
            }
        }
    };
    ($this:ident, $variant_name:ident, $that:ty, $fn_name:ident, $($pat:tt)*) => {
        impl Maybe<&$this> {
            fn $fn_name(&self) -> Maybe<&$that> {
                impl_enter!(@iflet self, $this, $variant_name, $($pat)*)
            }
        }
    };
}

impl_enter!(S, TranslationUnit, TranslationUnit, translation_unit, 1);
impl_enter!(TranslationUnit, Leaf, Unit, leaf, 1);
impl_enter!(Unit, Declaration, Declaration, declaration, 1);
impl_enter!(Declaration, Declaration, Rc<str>, identifier, 3);
impl_enter!(fmap, Declaration, Declaration, AssignmentExpression, expr, 5);
impl_enter!(AssignmentExpression, Expression, Expression, expr, 1);

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
        | Definition. Visibility #Token::Let Mutability #Token::Identifier MaybeType #Token::Operator AssignmentExpression
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

    ArgList
       -> Empty. #Token::OpenParen #Token::CloseParen
        | Args. #Token::OpenParen Arg MoreArgs #Token::CloseParen
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum ArgList {
            Args(Vec<Expression>)
        }
        ;

    MoreArgs
       -> Epsilon
        | #Token::Comma Arg
        ;

    Arg
       -> &Expression
        ;

    PrimaryExpression
       -> #Token::Identifier
        | Call. #Token::Identifier ArgList
        | #Token::Integer
        | #Token::Float
        | ConditionalExpression
        ;

    ConditionalExpression
       -> #Token::If Expression Block IfTail
       @ #[derive(Clone, Debug, PartialEq)]
       pub enum ConditionalExpression {
           Exprs(Vec<(Box<Expression>, Box<Block>)>, Option<Box<Block>>)
       }
       ;

    IfTail
       -> Epsilon
        | #Token::Elif Expression Block IfTail
        | #Token::Else Expression Block
        ;

    Expression
       -> PrimaryExpression
        | Op. #Token::Operator ExprList
        | Unary. #Token::Operator ExprList
        | Prefix. #Token::Operator Expression
        ;

    ExprList -> Expression | Expression ExprList
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum ExprList { List(Vec<Box<Expression>>) }
        ;

    AssignmentExpression
       -> Expression
       ;

    Block -> #Token::OpenBrace Statements #Token::CloseBrace
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Block { Statements(Vec<Box<Statement>>) }
        ;
    Statements -> Epsilon | Statement #Token::SemiColon Statements;
    Statement -> Declaration | Expression;

    Path -> #Token::Identifier;
}

pub type Iter<'a, 'b> = &'a mut Peekable<std::slice::Iter<'b, Token>>;
type PrecedenceMap = HashMap<Rc<str>, Precedence>;

fn default_bin_ops() -> PrecedenceMap {
    let mut precedence = HashMap::new();

    // Ternary
    precedence.insert(From::from("?"), Precedence::binop_right(1));
    precedence.insert(From::from(":"), Precedence::binop_right(1));

    // Logical operations: or, and, eq, neq, leq, meq, less, more
    precedence.insert(From::from("||"), Precedence::binop(2));
    precedence.insert(From::from("&&"), Precedence::binop(3));
    precedence.insert(From::from("=="), Precedence::binop(4));
    precedence.insert(From::from("!="), Precedence::binop(4));
    precedence.insert(From::from("<="), Precedence::binop(5));
    precedence.insert(From::from(">="), Precedence::binop(5));
    precedence.insert(From::from("<"), Precedence::binop(5));
    precedence.insert(From::from(">"), Precedence::binop(5));

    // Bitwise operations: or, xor, and, shl, shr, rotating bitshifts
    precedence.insert(From::from("|"), Precedence::binop(6));
    precedence.insert(From::from("^"), Precedence::binop(7));
    precedence.insert(From::from("&"), Precedence::binop(8));
    precedence.insert(From::from("<<"), Precedence::binop(9));
    precedence.insert(From::from(">>"), Precedence::binop(9));
    precedence.insert(From::from("<<<"), Precedence::binop(9));
    precedence.insert(From::from(">>>"), Precedence::binop(9));

    // Standard arithmetic: plus, minus, mod, times, div, pow
    precedence.insert(From::from("+"), Precedence::binop(10));
    precedence.insert(From::from("-"), Precedence::binop(10));
    precedence.insert(From::from("%"), Precedence::binop(11));
    precedence.insert(From::from("*"), Precedence::binop(11));
    precedence.insert(From::from("/"), Precedence::binop(11));
    precedence.insert(From::from("**"), Precedence::binop_right(12));

    precedence
}

fn default_unary_ops() -> PrecedenceMap {
    let mut precedence = HashMap::new();

    // Unary plus (nop), unary minus (negation), unary not (!= 0), bitwise not
    precedence.insert(From::from("+"), Precedence::unary());
    precedence.insert(From::from("-"), Precedence::unary());
    precedence.insert(From::from("!"), Precedence::unary());
    precedence.insert(From::from("~"), Precedence::unary());

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
    fn binop_right(precedence: usize) -> Precedence {
        Precedence { precedence, param_count: 2, left_associative: false }
    }
    fn unary() -> Precedence {
        Precedence { precedence: 1, param_count: 1, left_associative: true }
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
        AssignmentExpression::Expression(self.parse_expression())
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_expression_(1)
    }

    fn parse_expression_(&mut self, precedence: usize) -> Expression {
        match self.iter.peek() {
            Some(Token::Operator(p)) => match self.unary_precedence.get(p) {
                Some(n) if n.left_associative && n.param_count == 1 => {
                    assert_eq!(n.left_associative, true);
                    let op = self.iter.next().unwrap().clone();
                    let expr = self.parse_expression_(n.precedence);
                    return Expression::Unary(op, ExprList::List(vec![Box::new(expr)]));
                }
                _ => panic!("Unexpected operator: {:?}", p)
            }
            _ => {}
        }
        let mut expr = Expression::PrimaryExpression(self.parse_primary_expression());
        loop {
            match self.iter.peek() {
                Some(Token::Operator(p)) => match self.precedence.get(p).map(|t| *t) {
                    Some(n) if precedence <= n.precedence => {
                        let mut left = vec![Box::new(expr)];
                        let op = self.iter.next().unwrap().clone();
                        let mut tail = (1..n.param_count).into_iter()
                            .map(|_| self.parse_expression_(n.precedence))
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
        match self.iter.peek() {
            Some(Token::Identifier(..)) => {
                let t = self.iter.next().unwrap().clone();
                match self.iter.peek() {
                    Some(Token::OpenParen()) => PrimaryExpression::Call(t.clone(), self.parse_args()),
                    _ => PrimaryExpression::Identifier(t.clone())
                }
            }
            Some(Token::Integer(..)) => PrimaryExpression::Integer(self.iter.next().unwrap().clone()),
            Some(Token::Float(..)) => PrimaryExpression::Float(self.iter.next().unwrap().clone()),
            Some(Token::If(..)) => PrimaryExpression::ConditionalExpression(self.parse_if_expr()),
            t => unexpected_token!(t, self.iter)
        }
    }

    fn parse_if_expr(&mut self) -> ConditionalExpression {
        // if expr block [elif expr block ]* [else block]
        read_token!(self.iter, Token::If);
        let mut choices = Vec::new();
        let expr = self.parse_expression();
        let block = self.parse_block();
        let mut or_else = None;
        choices.push((Box::new(expr), Box::new(block)));
        loop {
            match self.iter.peek() {
                Some(Token::Elif(..)) => {
                    read_token!(self.iter, Token::Elif);
                    let expr = self.parse_expression();
                    let block = self.parse_block();
                    choices.push((Box::new(expr), Box::new(block)));
                }
                Some(Token::Else(..)) => {
                    read_token!(self.iter, Token::Else);
                    let block = self.parse_block();
                    or_else = Some(Box::new(block));
                    break;
                }
                _ => break
            }
        }
        ConditionalExpression::Exprs(choices, or_else)
    }

    fn parse_block(&mut self) -> Block {
        read_token!(self.iter, Token::OpenBrace);
        let mut stmts = Vec::new();
        loop {
            match self.iter.peek() {
                Some(Token::CloseBrace(..)) => {
                    break;
                }
                Some(_) => {
                    stmts.push(Box::new(self.parse_stmt()));
                }
                t => unexpected_token!(t, self.iter)
            }
        }
        read_token!(self.iter, Token::CloseBrace);
        Block::Statements(stmts)
    }

    fn parse_stmt(&mut self) -> Statement {
        let res = match_first!(
            self.iter.peek() => _t,
            default unexpected_token!(_t, self.iter),

            Declaration => Statement::Declaration(self.parse_declaration()),
            Expression => Statement::Expression(self.parse_expression()),
        );
        read_token!(self.iter, Token::SemiColon);
        res
    }

    fn parse_args(&mut self) -> ArgList {
        let mut args = Vec::new();

        read_token!(self.iter, Token::OpenParen);
        loop {
            match self.iter.peek() {
                Some(Token::CloseParen()) => {
                    self.iter.next();
                    break;
                }
                Some(_) => {
                    args.push(self.parse_expression());
                    match self.iter.peek() {
                        Some(Token::CloseParen()) => {
                            self.iter.next();
                            break;
                        }
                        Some(Token::Comma()) => {
                            self.iter.next();
                        }
                        t => unexpected_token!(t, self.iter)
                    }
                }
                t => unexpected_token!(t, self.iter)
            }
        }

        ArgList::Args(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    fn to_token(prec: &PrecedenceMap, s: &str) -> Token {
        prec.get(s)
            .map(|_| Token::Operator(From::from(s)))
            .unwrap_or_else(|| Token::Integer(s.parse().unwrap()))
    }

    macro_rules! eval_bin {
        ($list:ident, $op:tt) => {
            eval_tree(&*$list[0]) $op eval_tree(&*$list[1])
        }
    }

    fn eval_tree(expr: &Expression) -> i128 {
        match expr {
            Expression::PrimaryExpression(PrimaryExpression::Integer(Token::Integer(e))) => *e,
            Expression::PrimaryExpression(_) => unreachable!(),
            Expression::Op(op, ExprList::List(list)) => {
                let op_s: &str = if let Token::Operator(s) = op { &*s } else { unreachable!() };
                match op_s {
                    "+" => eval_bin!(list, +),
                    "-" => eval_bin!(list, -),
                    "*" => eval_bin!(list, *),
                    "/" => eval_bin!(list, /),
                    "&" => eval_bin!(list, &),
                    "|" => eval_bin!(list, |),
                    "^" => eval_bin!(list, ^),
                    "**" => eval_tree(&*list[0]).pow(eval_tree(&*list[1]) as u32),
                    "?" => {
                        if let Expression::Op(Token::Operator(op), ExprList::List(res_list)) = &*list[1] {
                            if &**op != ":" {
                                unreachable!();
                            }
                            if eval_tree(&*list[0]) != 0 {
                                eval_tree(&*res_list[0])
                            } else {
                                eval_tree(&*res_list[1])
                            }
                        } else {
                            unreachable!()
                        }
                    }
                    _ => unreachable!()
                }
            }
            Expression::Unary(op, ExprList::List(list)) => {
                let op_s: &str = if let Token::Operator(s) = op { &*s } else { unreachable!() };
                match op_s {
                    "-" => -eval_tree(&*list[0]),
                    "~" => !eval_tree(&*list[0]),
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
        let result = eval_tree(&context.parse_expression());
        println!("{} = {} (expected: {})", expr, result, expected);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_expr_and_eval() {
        check("0", 0);
        check("-1", -1);
        check("1 + 1", 2);
        check("1 + -1", 0);
        check("1 * -1", -1);
        check("1 + 2 * 2", 5);
        check("1 - 2 * 2", -3);
        check("2 ** 3", 8);
        check("- 2 ** 4", -16);
        check("1 ? 2 : 3", 2);
        check("0 ? 2 : 3", 3);
        check("1 ? 2 : 0 ? 3 : 4", 2); // no horses here
        check("0 ? 2 : 1 ? 3 : 4", 3);
        check("1 ? 2 : 1 ? 3 : 4", 2);
        check("0 ? 2 : 0 ? 3 : 4", 4);

        check("1 & 2", 0);
        check("1 & 3 | 4", 5);
        check("1 | 3 & 4", 1);
    }

    #[test]
    fn parse_fn_empty() {
        let s = parse(&mut vec![
                      Token::Let(),
                      Token::Identifier(From::from("bar")),
                      Token::Operator(From::from("=")),
                      Token::Identifier(From::from("foo")),
                      Token::OpenParen(),
                      Token::CloseParen(),
                      Token::SemiColon()].iter().peekable());
        assert_eq!(
            Just(&s).translation_unit().leaf().declaration().identifier().from_just(),
            &From::from("bar"));
        assert_eq!(
            Just(&s).translation_unit().leaf().declaration().expr().expr().from_just(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                    Token::Identifier(From::from("foo")), ArgList::Args(vec![]))));
    }

    #[test]
    fn parse_fn_one_arg() {
        let s = parse(&mut vec![
                      Token::Let(),
                      Token::Identifier(From::from("bar")),
                      Token::Operator(From::from("=")),
                      Token::Identifier(From::from("foo")),
                      Token::OpenParen(),
                      Token::Identifier(From::from("asd")),
                      Token::CloseParen(),
                      Token::SemiColon()].iter().peekable());
        assert_eq!(
            Just(&s).translation_unit().leaf().declaration().identifier().from_just(),
            &From::from("bar"));
        assert_eq!(
            Just(&s).translation_unit().leaf().declaration().expr().expr().from_just(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                    Token::Identifier(From::from("foo")), ArgList::Args(
                        vec![Expression::PrimaryExpression(PrimaryExpression::Identifier(Token::Identifier(From::from("asd"))))]
                        ))));
    }

    #[test]
    fn parse_fn_two_args() {
        let s = parse(&mut vec![
                      Token::Let(),
                      Token::Identifier(From::from("bar")),
                      Token::Operator(From::from("=")),
                      Token::Identifier(From::from("foo")),
                      Token::OpenParen(),
                      Token::Identifier(From::from("asd")),
                      Token::Comma(),
                      Token::Identifier(From::from("qwe")),
                      Token::CloseParen(),
                      Token::SemiColon()].iter().peekable());
        assert_eq!(
            Just(&s).translation_unit().leaf().declaration().identifier().from_just(),
            &From::from("bar"));
        assert_eq!(
            Just(&s).translation_unit().leaf().declaration().expr().expr().from_just(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                    Token::Identifier(From::from("foo")), ArgList::Args(
                        vec![Expression::PrimaryExpression(PrimaryExpression::Identifier(Token::Identifier(From::from("asd")))),
                       Expression::PrimaryExpression(PrimaryExpression::Identifier(Token::Identifier(From::from("qwe")))) ]
                        ))));
    }

    #[test]
    fn parse_fn_three_args() {
        let s = parse(&mut vec![
                      Token::Let(),
                      Token::Identifier(From::from("bar")),
                      Token::Operator(From::from("=")),
                      Token::Identifier(From::from("foo")),
                      Token::OpenParen(),
                      Token::Identifier(From::from("asd")),
                      Token::Comma(),
                      Token::Identifier(From::from("qwe")),
                      Token::Comma(),
                      Token::Identifier(From::from("aoeu")),
                      Token::CloseParen(),
                      Token::SemiColon()].iter().peekable());
        assert_eq!(
            Just(&s).translation_unit().leaf().declaration().identifier().from_just(),
            &From::from("bar"));
        assert_eq!(
            Just(&s).translation_unit().leaf().declaration().expr().expr().from_just(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                    Token::Identifier(From::from("foo")), ArgList::Args(
                        vec![Expression::PrimaryExpression(PrimaryExpression::Identifier(Token::Identifier(From::from("asd")))),
                       Expression::PrimaryExpression(PrimaryExpression::Identifier(Token::Identifier(From::from("qwe")))),
                       Expression::PrimaryExpression(PrimaryExpression::Identifier(Token::Identifier(From::from("aoeu")))) ]
                        ))));
    }
}
