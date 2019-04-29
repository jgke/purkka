use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::Peekable;
use std::rc::Rc;

use regex::Regex;

use fragment::fragment::{FragmentIterator, Source};
use purkkasyntax::*;
use purkkatoken::token::Token;

macro_rules! maybe_read_token {
    ($iter:expr, $tok:path) => {
        if let Some($tok(..)) = $iter.peek() {
            $iter.next()
        } else {
            None
        }
    };
}

macro_rules! unexpected_token {
    ($token:expr, $iter:expr) => {
        match $token {
            None => panic!("Unexpected end of file"),
            Some(t) => {
                let as_s = format!("{:?}", t);
                let num = t.get_num();
                panic!("Unexpected token: {}\n{}", as_s, $iter.fragment.source_to_str(&$iter.sources[num]));
            }
        }
    };
}

macro_rules! unexpected_token_expected_one {
    ($token:expr, $iter:expr, $expected:path) => {
        match $token {
            None => panic!("Unexpected end of file, expected {}", stringify!($expected)),
            Some(t) => {
                let as_s = format!("{:?}", t);
                let expected = stringify!($expected);
                let num = t.get_num();
                panic!("Unexpected token: {}, expected {}\n{}", as_s, expected, $iter.fragment.source_to_str(&$iter.sources[num]));
            }
        }
    };
}

macro_rules! read_token {
    ($iter:expr, $tok:path) => {
        if let Some($tok(..)) = $iter.peek() {
            $iter.next().unwrap().clone()
        } else {
            unexpected_token_expected_one!(&$iter.next(), $iter, $tok)
        }
    };
}

type PrecedenceMap = HashMap<Rc<str>, Precedence>;

fn default_bin_ops() -> PrecedenceMap {
    let mut precedence = HashMap::new();

    // Assignment
    precedence.insert(From::from("="), Precedence::binop_right(1));
    precedence.insert(From::from("&="), Precedence::binop_right(1));

    // Ternary
    precedence.insert(From::from("?"), Precedence::binop_right(2));
    precedence.insert(From::from(":"), Precedence::binop_right(2));

    // Logical operations: or, and, eq, neq, leq, meq, less, more
    precedence.insert(From::from("||"), Precedence::binop(3));
    precedence.insert(From::from("&&"), Precedence::binop(4));
    precedence.insert(From::from("=="), Precedence::binop(5));
    precedence.insert(From::from("!="), Precedence::binop(5));
    precedence.insert(From::from("<="), Precedence::binop(6));
    precedence.insert(From::from(">="), Precedence::binop(6));
    precedence.insert(From::from("<"), Precedence::binop(6));
    precedence.insert(From::from(">"), Precedence::binop(6));

    // Bitwise operations: or, xor, and, shl, shr, rotating bitshifts
    precedence.insert(From::from("|"), Precedence::binop(7));
    precedence.insert(From::from("^"), Precedence::binop(8));
    precedence.insert(From::from("&"), Precedence::binop(9));
    precedence.insert(From::from("<<"), Precedence::binop(10));
    precedence.insert(From::from(">>"), Precedence::binop(10));
    precedence.insert(From::from("<<<"), Precedence::binop(10));
    precedence.insert(From::from(">>>"), Precedence::binop(10));

    // Standard arithmetic: plus, minus, mod, times, div, pow
    precedence.insert(From::from("+"), Precedence::binop(11));
    precedence.insert(From::from("-"), Precedence::binop(11));
    precedence.insert(From::from("%"), Precedence::binop(12));
    precedence.insert(From::from("*"), Precedence::binop(12));
    precedence.insert(From::from("/"), Precedence::binop(12));
    precedence.insert(From::from("**"), Precedence::binop_right(13));

    precedence
}

fn default_unary_ops() -> PrecedenceMap {
    let mut precedence = HashMap::new();

    // Unary plus (nop), unary minus (negation), logical not (!= 0), bitwise not,
    // addressof, prefix increment/decrement
    precedence.insert(From::from("+"), Precedence::unary());
    precedence.insert(From::from("-"), Precedence::unary());
    precedence.insert(From::from("!"), Precedence::unary());
    precedence.insert(From::from("~"), Precedence::unary());
    precedence.insert(From::from("&"), Precedence::unary());
    precedence.insert(From::from("++"), Precedence::unary());
    precedence.insert(From::from("--"), Precedence::unary());

    precedence
}

fn default_postfix_ops() -> PrecedenceMap {
    let mut precedence = HashMap::new();

    // Postfix increment, decrement
    precedence.insert(From::from("++"), Precedence::unary_right());
    precedence.insert(From::from("--"), Precedence::unary_right());

    precedence
}

pub fn parse(iter: Iter, sources: &[Source], fragment_iter: &FragmentIterator) -> S {
    let mut context = ParseContext {
        unary_precedence: default_unary_ops(),
        postfix_precedence: default_postfix_ops(),
        precedence: default_bin_ops(),
        iter,
        fragment: fragment_iter,
        sources
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
        Precedence {
            precedence,
            param_count: 2,
            left_associative: true,
        }
    }
    fn binop_right(precedence: usize) -> Precedence {
        Precedence {
            precedence,
            param_count: 2,
            left_associative: false,
        }
    }
    fn unary() -> Precedence {
        Precedence {
            precedence: 1,
            param_count: 1,
            left_associative: true,
        }
    }
    fn unary_right() -> Precedence {
        Precedence {
            precedence: 1,
            param_count: 1,
            left_associative: false,
        }
    }
}

pub(crate) type Iter<'a, 'b> = &'a mut Peekable<std::slice::Iter<'b, Token>>;

struct ParseContext<'a, 'b> {
    unary_precedence: PrecedenceMap,
    postfix_precedence: PrecedenceMap,
    precedence: PrecedenceMap,
    iter: Iter<'a, 'b>,
    fragment: &'a FragmentIterator,
    sources: &'a [Source],
}

impl<'a, 'b> ParseContext<'a, 'b> {
    fn next(&mut self) -> Option<&'a Token> {
        self.iter.next()
    }
    fn peek(&mut self) -> Option<&&'a Token> {
        self.iter.peek()
    }
    fn parse_translation_unit(&mut self) -> TranslationUnit {
        let mut units = Vec::new();
        while maybe_read_token!(self, Token::SemiColon).is_some() {}
        while self.peek().is_some() {
            units.push(self.parse_unit());
            while maybe_read_token!(self, Token::SemiColon).is_some() {}
        }
        TranslationUnit::Units(units)
    }

    fn parse_unit(&mut self) -> Unit {
        match_first!(
            self.peek() => _t,
            default unexpected_token!(_t, self),

            Declaration => Unit::Declaration(Box::new(self.parse_declaration(true))),
            OperatorOverload => Unit::OperatorOverload(Box::new(self.parse_new_operator())),
            ImportFile => Unit::ImportFile(Box::new(self.parse_include())),
        )
    }

    fn parse_declaration(&mut self, semi: bool) -> Declaration {
        let visible = maybe_read_token!(self, Token::Pub).is_some();
        let mutable = match self.next() {
            Some(Token::Let(..)) => true,
            Some(Token::Const(..)) => false,
            Some(Token::Fun(..)) => {
                let ident = Some(read_token!(self, Token::Identifier))
                    .as_ref()
                    .identifier_s()
                    .unwrap()
                    .clone();
                let (params, return_type, block) = self.parse_fun();
                return Declaration::Declaration(
                    visible,
                    false,
                    ident,
                    None,
                    Some(Box::new(self.fun_to_expr(
                        params,
                        return_type,
                        block,
                    ))),
                );
            }
            t => unexpected_token!(t, self),
        };
        let ident = match self.next() {
            Some(Token::Identifier(_, s)) => s.clone(),
            t => unexpected_token!(t, self),
        };
        let ty = match self.peek() {
            Some(Token::Operator(_, t)) if &**t == ":" => {
                read_token!(self, Token::Operator);
                Some(Box::new(self.parse_type()))
            }
            _ => None,
        };

        let res = match self.peek() {
            Some(Token::Operator(_, t)) if &**t == "=" => {
                read_token!(self, Token::Operator);
                let expr = Box::new(self.parse_expression());
                Declaration::Declaration(visible, mutable, ident, ty, Some(expr))
            }
            _ => Declaration::Declaration(visible, mutable, ident, ty, None),
        };

        if semi {
            read_token!(self, Token::SemiColon);
        }
        res
    }

    fn parse_type(&mut self) -> TypeSignature {
        self.parse_type_(true)
    }

    fn parse_type_(&mut self, maybe_fn: bool) -> TypeSignature {
        let ref_regex = Regex::new(r"^(&|&\?)+$").unwrap();
        let ty = match self.peek() {
            /* int */
            /* int -> int */
            Some(Token::Identifier(..)) => {
                let t = self.next().identifier_s().unwrap().clone();
                TypeSignature::Plain(t)
            }
            /* (foo, bar: int) -> int */
            /* (int, int) */
            Some(Token::OpenParen(..)) => self.parse_fn_or_tuple(maybe_fn),
            /* struct { foo: int, bar: int } */
            Some(Token::Struct(..)) => self.parse_struct(),
            /* enum { foo, bar(int) } */
            Some(Token::Enum(..)) => self.parse_enum(),
            /* [int] */
            Some(Token::OpenBracket(..)) => self.parse_array(),
            /* &int, &?int */
            Some(Token::Operator(_, t)) if ref_regex.is_match(t) => self.parse_ptr(t),
            t => unexpected_token!(t, self),
        };
        if maybe_fn {
            self.maybe_parse_fn(ty)
        } else {
            ty
        }
    }

    fn parse_fn_or_tuple(&mut self, maybe_fn: bool) -> TypeSignature {
        let params = self.parse_param_list();
        match self.peek() {
            Some(Token::Operator(_, t)) if &**t == "->" && maybe_fn => {
                read_token!(self, Token::Operator);
                let return_type = self.parse_type();
                TypeSignature::Function(params, Box::new(return_type))
            }
            t => {
                let ty_list: Result<Vec<TypeSignature>, ()> = params
                    .clone()
                    .into_iter()
                    .map(TryFrom::try_from)
                    .collect();
                if ty_list.is_ok() {
                    TypeSignature::Tuple(ty_list.unwrap())
                } else {
                    unexpected_token!(t, self)
                }
            }
        }
    }

    fn parse_struct(&mut self) -> TypeSignature {
        read_token!(self, Token::Struct);
        let name = maybe_read_token!(self, Token::Identifier);
        read_token!(self, Token::OpenBrace);
        let fields = self.parse_struct_list();
        read_token!(self, Token::CloseBrace);
        TypeSignature::Struct(name.identifier_s().cloned(), fields)
    }

    fn parse_enum(&mut self) -> TypeSignature {
        read_token!(self, Token::Enum);
        let name = maybe_read_token!(self, Token::Identifier);
        read_token!(self, Token::OpenBrace);
        let fields = self.parse_enum_list();
        read_token!(self, Token::CloseBrace);
        TypeSignature::Enum(name.identifier_s().cloned(), fields)
    }

    fn parse_array(&mut self) -> TypeSignature {
        read_token!(self, Token::OpenBracket);
        let ty = self.parse_type();
        let expr = match self.peek() {
            Some(Token::SemiColon(..)) => {
                read_token!(self, Token::SemiColon);
                let lit = self.parse_literal();
                match lit {
                    Literal::Integer(Token::Integer(_, i)) => {
                        Some(TryFrom::try_from(i).unwrap())
                    }
                    _ => panic!("Not implemented: compile-time expr parsing"),
                }
            }
            _ => None,
        };
        read_token!(self, Token::CloseBracket);
        TypeSignature::Array(Box::new(ty), expr)
    }

    fn parse_ptr(&mut self, op: &str) -> TypeSignature {
        read_token!(self, Token::Operator);
        let mut ty = self.parse_type_(false);
        let mut ref_iter = op.chars().rev();
        loop {
            match ref_iter.next() {
                Some('?') => {
                    assert_eq!(ref_iter.next(), Some('&'));
                    ty = TypeSignature::Pointer {
                        nullable: true,
                        ty: Box::new(ty),
                    };
                }
                Some('&') => {
                    ty = TypeSignature::Pointer {
                        nullable: false,
                        ty: Box::new(ty),
                    }
                }
                None => break,
                _ => unreachable!(),
            }
        }
        ty
    }

    fn maybe_parse_fn(&mut self, arg_ty: TypeSignature) -> TypeSignature {
        match self.peek() {
            Some(Token::Operator(_, t)) if &**t == "->" => {
                read_token!(self, Token::Operator);
                let return_type = self.parse_type();
                TypeSignature::Function(vec![Param::Anon(Box::new(arg_ty))], Box::new(return_type))
            }
            _ => arg_ty,
        }
    }

    fn parse_param_list(&mut self) -> Vec<Param> {
        read_token!(self, Token::OpenParen);
        let res = self.parse_comma_delimited_to_vec(Self::parse_param);
        read_token!(self, Token::CloseParen);
        res
    }

    fn parse_param(&mut self) -> Option<Param> {
        match self.peek() {
            Some(Token::Identifier(_, ident)) => {
                let ty = self.parse_type();
                if let TypeSignature::Plain(_) = &ty {
                    match self.peek() {
                        Some(Token::Operator(_, t)) if &**t == ":" => {
                            self.next();
                            let ty = self.parse_type();
                            Some(Param::Param(ident.clone(), Box::new(ty)))
                        }
                        _ => Some(Param::Anon(Box::new(TypeSignature::Plain(ident.clone())))),
                    }
                } else {
                    Some(Param::Anon(Box::new(ty)))
                }
            }
            Some(_) => match_first!(
                    self.peek() => _t,
                    default None,

                    TypeSignature => Some(Param::Anon(Box::new(self.parse_type()))),),
            None => None,
        }
    }

    fn parse_struct_list(&mut self) -> Vec<StructField> {
        self.parse_comma_delimited_to_vec(Self::parse_struct_field)
    }

    fn parse_enum_list(&mut self) -> Vec<EnumField> {
        self.parse_comma_delimited_to_vec(Self::parse_enum_field)
    }

    fn parse_struct_field(&mut self) -> Option<StructField> {
        match self.peek() {
            Some(Token::Identifier(_, _)) => {
                let name = self.next().identifier_s().unwrap().clone();
                read_token!(self, Token::Operator);
                let ty = Box::new(self.parse_type());
                Some(StructField::Field { name, ty })
            }
            _ => None,
        }
    }

    fn parse_enum_field(&mut self) -> Option<EnumField> {
        match self.peek() {
            Some(Token::Identifier(_, _)) => {
                let name = self.next().identifier_s().unwrap().clone();
                let value = match self.peek() {
                    Some(Token::Operator(_, t)) if &**t == "=" => {
                        panic!("Not implemented");
                    }
                    _ => None,
                };
                let ty = match self.peek() {
                    Some(Token::OpenParen(..)) => Some(self.parse_type()),
                    _ => None,
                };
                Some(EnumField::Field { name, value, ty })
            }
            _ => None,
        }
    }

    fn parse_comma_delimited_to_vec<T>(&mut self, cb: fn(&mut Self) -> Option<T>) -> Vec<T> {
        let mut things = Vec::new();
        if let Some(p) = cb(self) {
            things.push(p);
        } else {
            return things;
        }
        while let Some(Token::Comma(..)) = self.peek() {
            read_token!(self, Token::Comma);
            match cb(self) {
                Some(p) => things.push(p),
                None => unexpected_token!(self.peek(), self),
            }
        }
        things
    }

    fn parse_new_operator(&mut self) -> OperatorOverload {
        read_token!(self, Token::NewOperator);
        let op = if let Token::StringLiteral(_, s) = read_token!(self, Token::StringLiteral) {
            s
        } else {
            unreachable!();
        };
        let (params, return_type, block) = self.parse_fun();
        let ty = TypeSignature::Function(params.clone(), Box::new(return_type.clone()));
        let body = self.fun_to_expr(params, return_type, block);
        OperatorOverload::OperatorOverload(op, Box::new(ty), Box::new(body))
    }

    fn parse_include(&mut self) -> ImportFile {
        read_token!(self, Token::Import);
        let ffi = match self.peek() {
            Some(Token::OpenParen(..)) => {
                read_token!(self, Token::OpenParen);
                let ffi = if let Token::Identifier(_, s) = read_token!(self, Token::Identifier) {
                    s
                } else {
                    unreachable!();
                };
                read_token!(self, Token::CloseParen);
                Some(ffi)
            }
            _ => None,
        };
        let file = if let Token::StringLiteral(_, s) = read_token!(self, Token::StringLiteral) {
            s
        } else {
            unreachable!();
        };
        ImportFile::Import(file, ffi)
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_expression_(1)
    }

    fn parse_expression_(&mut self, precedence: usize) -> Expression {
        if let Some(Token::Operator(_, p)) = self.peek() {
            match self.unary_precedence.get(p).copied() {
                Some(n) if n.left_associative => {
                    assert_eq!(n.left_associative, true);
                    let op = self.next().unwrap().clone();
                    let exprs = (0..n.param_count)
                        .map(|_| self.parse_expression_(n.precedence))
                        .collect();
                    return Expression::Unary(op, ExprList::List(exprs));
                }
                _ => panic!("Unknown prefix operator: {:?}", p),
            }
        }
        let mut expr = Expression::PrimaryExpression(self.parse_primary_expression());
        while let Some(Token::Operator(_, p)) = self.peek() {
            match self.precedence.get(p).copied() {
                Some(n) if precedence <= n.precedence => {
                    let mut left = vec![expr];
                    let op = self.next().unwrap().clone();
                    let mut tail = (1..n.param_count)
                        .map(|_| self.parse_expression_(n.precedence))
                        .collect();
                    left.append(&mut tail);
                    expr = Expression::Op(op, ExprList::List(left));
                }
                Some(_) => break,
                None => {
                    match self.postfix_precedence.get(p) {
                        Some(_) => {
                            let op = self.next().unwrap().clone();
                            expr = Expression::PostFix(Box::new(expr), op);
                        }
                        _ => panic!("Unknown operator: {}", p),
                    }
                }
            }
        }
        expr
    }

    fn parse_primary_expression(&mut self) -> PrimaryExpression {
        let mut expr = match self.peek() {
            Some(Token::Identifier(..)) => {
                let t = self.next().identifier_s().unwrap().clone();
                match self.peek() {
                    Some(Token::OpenParen(..)) => PrimaryExpression::Call(t, self.parse_args()),
                    _ => PrimaryExpression::Identifier(t),
                }
            }
            Some(Token::Integer(..)) | Some(Token::Float(..)) | Some(Token::StringLiteral(..)) => {
                PrimaryExpression::Literal(self.parse_literal())
            }
            Some(Token::If(..)) | Some(Token::While(..)) => {
                PrimaryExpression::BlockExpression(Box::new(self.parse_block_expression()))
            }
            Some(Token::OpenParen(..)) => {
                read_token!(self, Token::OpenParen);
                let expr = PrimaryExpression::Expression(Box::new(self.parse_expression()));
                read_token!(self, Token::CloseParen);
                expr
            }
            Some(Token::Fun(..)) => {
                read_token!(self, Token::Fun);
                let (params, return_type, block) = self.parse_fun();
                PrimaryExpression::Lambda(Lambda::Lambda(params, return_type, block))
            }
            t => unexpected_token!(t, self),
        };
        while let Some(Token::OpenBracket(..)) = self.peek() {
            read_token!(self, Token::OpenBracket);
            let inner_expr = Box::new(self.parse_expression());
            read_token!(self, Token::CloseBracket);
            expr = PrimaryExpression::ArrayAccess(Box::new(expr), inner_expr);
        }
        expr
    }

    fn parse_fun(&mut self) -> (Vec<Param>, TypeSignature, BlockExpression) {
        let params = self.parse_param_list();
        let return_type = match self.peek() {
            Some(Token::Operator(_, t)) if &**t == "->" => {
                read_token!(self, Token::Operator);
                self.parse_type()
            }
            _ => TypeSignature::Infer,
        };
        let block = match self.peek() {
            Some(Token::Operator(_, t)) if &**t == "=>" => {
                read_token!(self, Token::Operator);
                let expr = self.parse_expression();
                read_token!(self, Token::SemiColon);
                BlockExpression::Block(Block::Statements(vec![Statement::Return(Some(
                    Box::new(expr),
                ))]))
            }
            _ => self.parse_block_expression(),
        };
        (params, return_type, block)
    }

    fn fun_to_expr(
        &self,
        params: Vec<Param>,
        return_type: TypeSignature,
        block: BlockExpression,
    ) -> Expression {
        Expression::PrimaryExpression(PrimaryExpression::Lambda(Lambda::Lambda(
            params,
            return_type,
            block,
        )))
    }

    fn parse_block_expression(&mut self) -> BlockExpression {
        match self.peek() {
            Some(Token::If(..)) => self.parse_if_expr(),
            Some(Token::While(..)) => self.parse_while_expr(),
            Some(Token::For(..)) => self.parse_for_expr(),
            Some(Token::OpenBrace(..)) => BlockExpression::Block(self.parse_block()),
            t => unexpected_token!(t, self),
        }
    }

    fn parse_while_expr(&mut self) -> BlockExpression {
        // while expr block [else block]
        read_token!(self, Token::While);
        let expr = self.parse_expression();
        let block = self.parse_block();
        let else_block = match self.peek() {
            Some(Token::Else(..)) => {
                read_token!(self, Token::Else);
                Some(Box::new(self.parse_block()))
            }
            _ => None
        };
        BlockExpression::While(Box::new(expr), Box::new(block), else_block)
    }

    fn parse_for_expr(&mut self) -> BlockExpression {
        // for (maybe-statement ; maybe-statement ; maybe-statement) block [else block]
        read_token!(self, Token::For);

        read_token!(self, Token::OpenParen);

        let init = match self.peek() {
            Some(Token::SemiColon(..)) => None,
            _ => Some(Box::new(self.parse_statement(false)))
        };
        read_token!(self, Token::SemiColon);

        let cond = match self.peek() {
            Some(Token::SemiColon(..)) => None,
            _ => Some(Box::new(self.parse_statement(false)))
        };
        read_token!(self, Token::SemiColon);

        let post_loop = match self.peek() {
            Some(Token::CloseParen(..)) => None,
            _ => Some(Box::new(self.parse_statement(false)))
        };

        read_token!(self, Token::CloseParen);

        let block = self.parse_block();
        let else_block = match self.peek() {
            Some(Token::Else(..)) => {
                read_token!(self, Token::Else);
                Some(Box::new(self.parse_block()))
            }
            _ => None
        };
        BlockExpression::For(init, cond, post_loop, Box::new(block), else_block)
    }


    fn parse_literal(&mut self) -> Literal {
        match self.peek() {
            Some(Token::Integer(..)) => Literal::Integer(self.next().unwrap().clone()),
            Some(Token::Float(..)) => Literal::Float(self.next().unwrap().clone()),
            Some(Token::StringLiteral(..)) => Literal::StringLiteral(self.next().unwrap().clone()),
            t => unexpected_token!(t, self),
        }
    }

    fn parse_if_expr(&mut self) -> BlockExpression {
        // if expr block [elif expr block ]* [else block]
        read_token!(self, Token::If);
        let mut choices = Vec::new();
        let expr = self.parse_expression();
        let block = self.parse_block();
        let mut or_else = None;
        choices.push((Box::new(expr), Box::new(block)));
        loop {
            match self.peek() {
                Some(Token::Elif(..)) => {
                    read_token!(self, Token::Elif);
                    let expr = self.parse_expression();
                    let block = self.parse_block();
                    choices.push((Box::new(expr), Box::new(block)));
                }
                Some(Token::Else(..)) => {
                    read_token!(self, Token::Else);
                    let block = self.parse_block();
                    or_else = Some(Box::new(block));
                    break;
                }
                _ => break,
            }
        }
        BlockExpression::If(choices, or_else)
    }

    fn parse_block(&mut self) -> Block {
        if maybe_read_token!(self, Token::SemiColon).is_some() {
            return Block::Statements(vec![]);
        }
        read_token!(self, Token::OpenBrace);
        let mut stmts = Vec::new();
        loop {
            match self.peek() {
                Some(Token::CloseBrace(..)) => {
                    break;
                }
                Some(Token::SemiColon(..)) => {
                    read_token!(self, Token::SemiColon);
                }
                Some(_) => {
                    stmts.push(self.parse_statement(true));
                }
                t => unexpected_token!(t, self),
            }
        }
        read_token!(self, Token::CloseBrace);
        Block::Statements(stmts)
    }

    #[allow(unreachable_patterns)]
    fn parse_statement(&mut self, semi: bool) -> Statement {
        let res = match_first!(
            self.peek() => _t,
            default unexpected_token!(_t, self),

            // directly return the block_expr since it doesn't need a semicolon
            BlockExpression => return Statement::BlockExpression(Box::new(self.parse_block_expression())),
            Declaration => return Statement::Declaration(Box::new(self.parse_declaration(semi))),
            // Expressions can contain BlockExpressions, so this is
            // partially unreachable
            Expression => Statement::Expression(Box::new(self.parse_expression())),
            ReturnStatement => {
                read_token!(self, Token::Return);
                let expr = match self.peek() {
                    Some(Token::SemiColon(..)) => None,
                    _ => Some(Box::new(self.parse_expression()))
                };
                Statement::Return(expr)
            },
        );
        if semi {
            read_token!(self, Token::SemiColon);
        }
        res
    }

    fn parse_args(&mut self) -> ArgList {
        let mut args = Vec::new();

        read_token!(self, Token::OpenParen);
        loop {
            match self.peek() {
                Some(Token::CloseParen(..)) => {
                    read_token!(self, Token::CloseParen);
                    break;
                }
                Some(_) => {
                    args.push(self.parse_expression());
                    match self.peek() {
                        Some(Token::CloseParen(..)) => {
                            read_token!(self, Token::CloseParen);
                            break;
                        }
                        Some(Token::Comma(..)) => {
                            read_token!(self, Token::Comma);
                        }
                        t => unexpected_token!(t, self),
                    }
                }
                t => unexpected_token!(t, self),
            }
        }

        ArgList::Args(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use purkkatoken::token::Token;

    fn to_token(prec: &PrecedenceMap, s: &str) -> Token {
        prec.get(s)
            .map(|_| Token::Operator(0, From::from(s)))
            .unwrap_or_else(|| Token::Integer(0, s.parse().unwrap()))
    }

    macro_rules! eval_bin {
        ($list:ident, $op:tt) => {
            eval_tree(&$list[0]) $op eval_tree(&$list[1])
        }
    }

    fn eval_tree(expr: &Expression) -> i128 {
        match expr {
            Expression::PrimaryExpression(PrimaryExpression::Literal(Literal::Integer(
                Token::Integer(0, e),
            ))) => *e,
            Expression::PrimaryExpression(_) => unreachable!(),
            Expression::Op(op, ExprList::List(list)) => {
                let op_s: &str = if let Token::Operator(_, s) = op {
                    &*s
                } else {
                    unreachable!()
                };
                match op_s {
                    "+" => eval_bin!(list, +),
                    "-" => eval_bin!(list, -),
                    "*" => eval_bin!(list, *),
                    "/" => eval_bin!(list, /),
                    "&" => eval_bin!(list, &),
                    "|" => eval_bin!(list, |),
                    "^" => eval_bin!(list, ^),
                    "**" => eval_tree(&list[0]).pow(eval_tree(&list[1]) as u32),
                    "?" => {
                        if let Expression::Op(Token::Operator(_, op), ExprList::List(res_list)) =
                            &list[1]
                        {
                            if &**op != ":" {
                                unreachable!();
                            }
                            if eval_tree(&list[0]) != 0 {
                                eval_tree(&res_list[0])
                            } else {
                                eval_tree(&res_list[1])
                            }
                        } else {
                            unreachable!()
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Expression::Unary(op, ExprList::List(list)) => {
                let op_s: &str = if let Token::Operator(_, s) = op {
                    &*s
                } else {
                    unreachable!()
                };
                match op_s {
                    "-" => -eval_tree(&list[0]),
                    "~" => !eval_tree(&list[0]),
                    _ => unreachable!(),
                }
            }
            Expression::PostFix(expr, op) => {
                let op_s: &str = if let Token::Operator(_, s) = op {
                    &*s
                } else {
                    unreachable!()
                };
                match op_s {
                    "++" => eval_tree(&expr),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn check(expr: &str, expected: i128) {
        let unary_precedence = default_unary_ops();
        let postfix_precedence = default_unary_ops();
        let precedence = default_bin_ops();
        let both = unary_precedence
            .clone()
            .into_iter()
            .chain(precedence.clone().into_iter())
            .collect();
        let vec: Vec<Token> = expr.split(' ').map(|t| to_token(&both, t)).collect();
        let mut context = ParseContext {
            precedence,
            unary_precedence,
            postfix_precedence,
            iter: &mut vec.iter().peekable(),
            fragment: &FragmentIterator::new("", ""),
            sources: &Vec::new(),
        };
        let result = eval_tree(&context.parse_expression());
        println!("{} = {} (expected: {})", expr, result, expected);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_expr_and_eval() {
        check("0", 0);
        check("- 1", -1);
        check("1 + 1", 2);
        check("1 & ~ 1", 0);
        check("1 + - 1", 0);
        check("1 * - 1", -1);
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

        check("0 ++", 0);

        check("1 & 2", 0);
        check("1 & 3 | 4", 5);
        check("1 | 3 & 4", 1);
    }

    fn test_parse(list: Vec<Token>) -> S {
        parse(
            &mut list.iter().peekable(),
            &vec![],
            &FragmentIterator::new("", ""),
        )
    }

    #[test]
    fn parse_fn_empty() {
        let s = test_parse(
            vec![
                Token::Let(0),
                Token::Identifier(1, From::from("bar")),
                Token::Operator(2, From::from("=")),
                Token::Identifier(3, From::from("foo")),
                Token::OpenParen(4),
                Token::CloseParen(5),
                Token::SemiColon(6),
            ]
        );
        assert_eq!(
            Some(&s)
                .translation_unit()
                .units()
                .and_then(|t| t.get(0))
                .declaration()
                .identifier()
                .unwrap(),
            &From::from("bar")
        );
        assert_eq!(
            Some(&s)
                .translation_unit()
                .units()
                .and_then(|t| t.get(0))
                .declaration()
                .expr()
                .unwrap(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                From::from("foo"),
                ArgList::Args(vec![])
            ))
        );
    }

    #[test]
    fn parse_fn_one_arg() {
        let s = test_parse(
            vec![
                Token::Let(0),
                Token::Identifier(1, From::from("bar")),
                Token::Operator(2, From::from("=")),
                Token::Identifier(3, From::from("foo")),
                Token::OpenParen(4),
                Token::Identifier(5, From::from("asd")),
                Token::CloseParen(6),
                Token::SemiColon(7),
            ]
        );
        assert_eq!(
            Some(&s)
                .translation_unit()
                .units()
                .and_then(|t| t.get(0))
                .declaration()
                .identifier()
                .unwrap(),
            &From::from("bar")
        );
        assert_eq!(
            Some(&s)
                .translation_unit()
                .units()
                .and_then(|t| t.get(0))
                .declaration()
                .expr()
                .unwrap(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                From::from("foo"),
                ArgList::Args(vec![Expression::PrimaryExpression(
                    PrimaryExpression::Identifier(From::from("asd"))
                )])
            ))
        );
    }

    #[test]
    fn parse_fn_two_args() {
        let s = test_parse(
            vec![
                Token::Let(0),
                Token::Identifier(1, From::from("bar")),
                Token::Operator(2, From::from("=")),
                Token::Identifier(3, From::from("foo")),
                Token::OpenParen(4),
                Token::Identifier(5, From::from("asd")),
                Token::Comma(6),
                Token::Identifier(7, From::from("qwe")),
                Token::CloseParen(8),
                Token::SemiColon(9),
            ]
        );
        assert_eq!(
            Some(&s)
                .translation_unit()
                .units()
                .and_then(|t| t.get(0))
                .declaration()
                .identifier()
                .unwrap(),
            &From::from("bar")
        );
        assert_eq!(
            Some(&s)
                .translation_unit()
                .units()
                .and_then(|t| t.get(0))
                .declaration()
                .expr()
                .unwrap(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                From::from("foo"),
                ArgList::Args(vec![
                    Expression::PrimaryExpression(PrimaryExpression::Identifier(From::from("asd"))),
                    Expression::PrimaryExpression(PrimaryExpression::Identifier(From::from("qwe")))
                ])
            ))
        );
    }

    #[test]
    fn parse_fn_three_args() {
        let s = test_parse(
            vec![
                Token::Let(1),
                Token::Identifier(2, From::from("bar")),
                Token::Operator(3, From::from("=")),
                Token::Identifier(4, From::from("foo")),
                Token::OpenParen(5),
                Token::Identifier(6, From::from("asd")),
                Token::Comma(7),
                Token::Identifier(8, From::from("qwe")),
                Token::Comma(9),
                Token::Identifier(10, From::from("aoeu")),
                Token::CloseParen(11),
                Token::SemiColon(12),
            ]
        );
        assert_eq!(
            Some(&s)
                .translation_unit()
                .units()
                .and_then(|t| t.get(0))
                .declaration()
                .identifier()
                .unwrap(),
            &From::from("bar")
        );
        assert_eq!(
            Some(&s)
                .translation_unit()
                .units()
                .and_then(|t| t.get(0))
                .declaration()
                .expr()
                .unwrap(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                From::from("foo"),
                ArgList::Args(vec![
                    Expression::PrimaryExpression(PrimaryExpression::Identifier(From::from("asd"))),
                    Expression::PrimaryExpression(PrimaryExpression::Identifier(From::from("qwe"))),
                    Expression::PrimaryExpression(PrimaryExpression::Identifier(From::from(
                        "aoeu"
                    )))
                ])
            ))
        );
    }
}
