use std::convert::TryFrom;
use std::collections::HashMap;
use std::iter::Peekable;
use std::rc::Rc;

use crate::token::Token;

// 'Re-implement' Option<> because
//  a) error[E0116]: cannot define inherent `impl` for a type outside of the crate where the type is defined
//  b) I don't want to create a new trait for every single AST node (although this might be a good
//      idea in the future)
//  c) This gives some way for future
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Maybe<T> {
    Just(T),
    Nothing,
}

use Maybe::*;


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

impl<T> Maybe<T> {
    fn from_just(self) -> T {
        if let Just(t) = self {
            t
        } else {
            panic!()
        }
    }
    fn is_just(&self) -> bool {
        if let Just(_) = self {
            true
        } else {
            false
        }
    }
    fn as_ref(&self) -> Maybe<&T> {
        match *self {
            Just(ref x) => Just(x),
            Nothing => Nothing,
        }
    }
    fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Maybe<U> {
        match self {
            Just(x) => Just(f(x)),
            Nothing => Nothing,
        }
    }
}

macro_rules! maybe_read_token {
    ($iter:expr, $tok:path) => {
        if let Just($tok(..)) = $iter.peek() {
            From::from($iter.next())
        } else {
            Nothing
        }
    };
}

macro_rules! unexpected_token {
    ($token:expr, $iter:expr) => {
        match $token {
            Nothing => panic!("Unexpected end of file"),
            Just(t) => panic!("Unexpected token: {:?}", t),
        }
    };
}

macro_rules! read_token {
    ($iter:expr, $tok:path) => {
        if let Just($tok(..)) = $iter.peek() {
            $iter.next().from_just().clone()
        } else {
            unexpected_token!(&$iter.next(), $iter)
        }
    };
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
impl_enter!(Token, Identifier, Rc<str>, identifier_s, 1);
impl_enter!(
    fmap,
    Declaration,
    Declaration,
    AssignmentExpression,
    expr,
    5
);
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
        | Typedef
        ;

    Declaration
       -> Declaration. Visibility #Token::Let Mutability #Token::Identifier MaybeType
        | Definition. Visibility #Token::Let Mutability #Token::Identifier MaybeType #Token::Operator AssignmentExpression
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Declaration { Declaration(bool, bool, Rc<str>, Maybe<TypeSignature>, Maybe<AssignmentExpression>) }
        ;

    Typedef
       -> Newtype. Visibility #Token::Type #Token::Identifier TypeSignature
        ;

    MaybeType
       -> Type. #Token::Operator TypeSignature
        | NoType. Epsilon
        ;

    TypeSignature
          /* int */
       -> #Token::Identifier
          /* struct { foo: int } */
        | #Token::Struct MaybeIdentifier #Token::OpenBrace StructFieldList #Token::CloseBrace
          /* enum { foo, bar(int) } */
        | #Token::Enum MaybeIdentifier #Token::OpenBrace EnumFieldList #Token::CloseBrace
          /* [int] */
        | Array. #Token::OpenBracket TypeSignature #Token::CloseBracket
          /* *int, &int */
        | Pointer. #Token::Operator TypeSignature
          /* (foo, bar: int) -> int */
        | Function. #Token::OpenParen ParamList #Token::CloseParen #Token::Operator TypeSignature
          /* int -> int */
        | SingleParameterFunction. #Token::Identifier #Token::Operator TypeSignature
          /* (int, int) */
        | #Token::OpenParen TupleList #Token::CloseParen
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum TypeSignature {
            Plain(Rc<str>),
            Pointer { nullable: bool, ty: Box<TypeSignature> },

            Struct(Maybe<Rc<str>>, Vec<StructField>),
            Enum(Maybe<Rc<str>>, Vec<EnumField>),
            Tuple(Vec<Box<TypeSignature>>),
            Array(Box<TypeSignature>),

            Function(Vec<Param>, Box<TypeSignature>),
        }
        ;

    TupleList
       -> Epsilon
        | Last. TypeSignature
        | TypeSignature #Token::Comma ParamList
        ;

    ParamList
       -> Epsilon
        | Last. Param
        | Param #Token::Comma ParamList
        ;

    Param
       -> #Token::Identifier #Token::Operator TypeSignature
        | TypeSignature
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Param {
            Param(Rc<str>, Box<TypeSignature>),
            Anon(Box<TypeSignature>),
        }
        ;

    StructFieldList
       -> Epsilon
        | Last. StructField TrailingComma
        | StructField #Token::Comma StructFieldList
        ;

    EnumFieldList
       -> Epsilon
        | Last. EnumField TrailingComma
        | EnumField #Token::Comma EnumFieldList
        ;

    StructField
       -> #Token::Identifier #Token::Operator TypeSignature
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum StructField {
            Field { name: Rc<str>, ty: Box<TypeSignature> }
        }
        ;

    EnumField
       -> #Token::Identifier
        | #Token::Identifier #Token::Operator TypeSignature
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum EnumField {
            Field { name: Rc<str>, value: Maybe<Expression>, ty: Maybe<TypeSignature> }
        }
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
           Exprs(Vec<(Box<Expression>, Box<Block>)>, Maybe<Box<Block>>)
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

    TrailingComma -> #Token::Comma | Epsilon;
    MaybeIdentifier -> #Token::Identifier | Epsilon;
}

impl TryFrom<Param> for TypeSignature {
    type Error = ();

    fn try_from(param: Param) -> Result<Self, Self::Error> {
        match param {
            Param::Anon(ty) => Ok(*ty),
            Param::Param(..) => Err(())
        }
    }
}

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
        iter,
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
}

pub(crate) type Iter<'a, 'b> = &'a mut Peekable<std::slice::Iter<'b, Token>>;

struct ParseContext<'a, 'b> {
    unary_precedence: PrecedenceMap,
    precedence: PrecedenceMap,
    iter: Iter<'a, 'b>,
}

impl<'a, 'b> ParseContext<'a, 'b> {
    fn next(&mut self) -> Maybe<&'a Token> {
        From::from(self.iter.next())
    }
    fn peek(&mut self) -> Maybe<&&'a Token> {
        From::from(self.iter.peek())
    }
    fn parse_translation_unit(&mut self) -> TranslationUnit {
        let unit = self.parse_unit();
        let sc = read_token!(self, Token::SemiColon);
        let has_more = self.peek().is_just();
        match has_more {
            true => TranslationUnit::List(unit, sc, Box::new(self.parse_translation_unit())),
            false => TranslationUnit::Leaf(unit, sc),
        }
    }

    fn parse_unit(&mut self) -> Unit {
        match_first!(
            self.peek() => _t,
            default unexpected_token!(_t, self),

            Declaration => Unit::Declaration(self.parse_declaration()),
            IncludeFile => Unit::IncludeFile(self.parse_include()),
        )
    }

    fn parse_declaration(&mut self) -> Declaration {
        let visible = maybe_read_token!(self, Token::Pub).is_just();
        let mutable = match self.next() {
            Just(Token::Let()) => true,
            Just(Token::Const()) => false,
            t => unexpected_token!(t, self),
        };
        let ident = match self.next() {
            Just(Token::Identifier(s)) => s.clone(),
            t => unexpected_token!(t, self),
        };
        let ty = match self.peek() {
            Just(Token::Operator(t)) if &**t == ":" => {
                self.next();
                Just(self.parse_type())
            }
            _ => Nothing,
        };

        match self.peek() {
            Just(Token::Operator(t)) if &**t == "=" => {
                self.next();
                let expr = self.parse_assignment_expr();
                Declaration::Declaration(visible, mutable, ident, ty, Just(expr))
            }
            _ => Declaration::Declaration(visible, mutable, ident, ty, Nothing),
        }
    }

    fn parse_type(&mut self) -> TypeSignature {
        match self.peek() {
            /* int */
            /* int -> int */
            Just(Token::Identifier(..)) => {
                let t = self.next().identifier_s().from_just().clone();
                self.maybe_parse_fn(TypeSignature::Plain(t))
            }
            /* (foo, bar: int) -> int */
            /* (int, int) */
            Just(Token::OpenParen(..)) => {
                read_token!(self, Token::OpenParen);
                let params = self.parse_param_list();
                read_token!(self, Token::CloseParen);
                match self.peek() {
                    Just(Token::Operator(t)) if &**t == "->" => {
                        read_token!(self, Token::Operator);
                        let return_type = self.parse_type();
                        TypeSignature::Function(params, Box::new(return_type))
                    }
                    t => {
                        let ty_list: Result<Vec<Box<TypeSignature>>, ()> = params.clone().into_iter()
                            .map(TryFrom::try_from)
                            .map(|t| t.map(Box::new))
                            .collect();
                        if ty_list.is_ok() {
                            TypeSignature::Tuple(ty_list.unwrap())
                        } else {
                            unexpected_token!(t, self)
                        }
                    }
                }
            }
            /* struct { foo: int, bar: int } */
            Just(Token::Struct(..)) => {
                read_token!(self, Token::Struct);
                let name = maybe_read_token!(self, Token::Identifier);
                read_token!(self, Token::OpenBrace);
                let fields = self.parse_struct_list();
                read_token!(self, Token::CloseBrace);
                let struct_ty = TypeSignature::Struct(name.identifier_s().map(|t| t.clone()), fields);
                self.maybe_parse_fn(struct_ty)
            }
            /* enum { foo, bar(int) } */
            Just(Token::Enum(..)) => {
                read_token!(self, Token::Enum);
                let name = maybe_read_token!(self, Token::Identifier);
                read_token!(self, Token::OpenBrace);
                let fields = self.parse_enum_list();
                read_token!(self, Token::CloseBrace);
                let struct_ty = TypeSignature::Enum(name.identifier_s().map(|t| t.clone()), fields);
                self.maybe_parse_fn(struct_ty)
            }
            /* [int] */
            /* *int, &int */
            /* int -> int */
            t => unexpected_token!(t, self),
        }
    }

    fn maybe_parse_fn(&mut self, arg_ty: TypeSignature) -> TypeSignature {
        match self.peek() {
            Just(Token::Operator(t)) if &**t == "->" => {
                read_token!(self, Token::Operator);
                let return_type = self.parse_type();
                TypeSignature::Function(vec![Param::Anon(Box::new(arg_ty))], Box::new(return_type))
            }
            _ => arg_ty
        }
    }

    fn parse_param_list(&mut self) -> Vec<Param> {
        self.parse_comma_delimited_to_vec(Self::parse_param)
    }

    fn parse_param(&mut self) -> Maybe<Param> {
        match self.peek() {
            Just(Token::Identifier(ident)) => {
                self.next();
                match self.peek() {
                    Just(Token::Operator(t)) if &**t == ":" => {
                        self.next();
                        let ty = self.parse_type();
                        Just(Param::Param(ident.clone(), Box::new(ty)))
                    }
                    /* This is an ambiguous parse (could be either variable identifier (a -> a) or
                     * a type (int -> int)), but prefer parsing a type over identifier here, as
                     * this is in type signatures rather than lambda parameter lists */
                    _ => Just(Param::Anon(Box::new(TypeSignature::Plain(ident.clone()))))
                }
            }
            Just(_) => {
                match_first!(
                    self.peek() => _t,
                    default Nothing,

                    TypeSignature => Just(Param::Anon(Box::new(self.parse_type()))),)
            }
            Nothing => Nothing
        }
    }

    fn parse_struct_list(&mut self) -> Vec<StructField> {
        self.parse_comma_delimited_to_vec(Self::parse_struct_field)
    }

    fn parse_enum_list(&mut self) -> Vec<EnumField> {
        self.parse_comma_delimited_to_vec(Self::parse_enum_field)
    }

    fn parse_struct_field(&mut self) -> Maybe<StructField> {
        match self.peek() {
            Just(Token::Identifier(_)) => {
                let name = self.next().identifier_s().from_just().clone();
                read_token!(self, Token::Operator);
                let ty = Box::new(self.parse_type());
                Just(StructField::Field { name, ty })
            }
            _ => Nothing
        }
    }

    fn parse_enum_field(&mut self) -> Maybe<EnumField> {
        match self.peek() {
            Just(Token::Identifier(_)) => {
                let name = self.next().identifier_s().from_just().clone();
                let value = match self.peek() {
                    Just(Token::Operator(t)) if &**t == "=" => {
                        panic!("Not implemented");
                    }
                    _ => Nothing
                };
                let ty = match self.peek() {
                    Just(Token::OpenParen(..)) => {
                        Just(self.parse_type())
                    }
                    _ => Nothing
                };
                Just(EnumField::Field { name, value, ty })
            }
            _ => Nothing
        }
    }

    fn parse_comma_delimited_to_vec<T>(&mut self, cb: fn(&mut Self) -> Maybe<T>) -> Vec<T> {
        let mut things = Vec::new();
        if let Just(p) = cb(self) {
            things.push(p);
        } else {
            return things
        }
        while let Just(Token::Comma()) = self.peek() {
            read_token!(self, Token::Comma);
            match cb(self) {
                Just(p) => things.push(p),
                Nothing => unexpected_token!(self.peek(), self),
            }
        }
        things
    }


    fn parse_include(&mut self) -> IncludeFile {
        unexpected_token!(self.peek(), self)
    }

    fn parse_assignment_expr(&mut self) -> AssignmentExpression {
        AssignmentExpression::Expression(self.parse_expression())
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_expression_(1)
    }

    fn parse_expression_(&mut self, precedence: usize) -> Expression {
        match self.peek() {
            Just(Token::Operator(p)) => match self.unary_precedence.get(p) {
                Some(n) if n.left_associative && n.param_count == 1 => {
                    assert_eq!(n.left_associative, true);
                    let precedence = n.precedence;
                    let op = self.next().from_just().clone();
                    let expr = self.parse_expression_(precedence);
                    return Expression::Unary(op, ExprList::List(vec![Box::new(expr)]));
                }
                _ => panic!("Unexpected operator: {:?}", p),
            },
            _ => {}
        }
        let mut expr = Expression::PrimaryExpression(self.parse_primary_expression());
        loop {
            match self.peek() {
                Just(Token::Operator(p)) => match self.precedence.get(p).map(|t| *t) {
                    Some(n) if precedence <= n.precedence => {
                        let mut left = vec![Box::new(expr)];
                        let op = self.next().from_just().clone();
                        let mut tail = (1..n.param_count)
                            .into_iter()
                            .map(|_| self.parse_expression_(n.precedence))
                            .map(Box::new)
                            .collect();
                        left.append(&mut tail);
                        expr = Expression::Op(op, ExprList::List(left));
                    }
                    Some(_) => break,
                    None => panic!("Unknown operator: {}", p),
                },
                _ => break,
            };
        }
        expr
    }
    fn parse_primary_expression(&mut self) -> PrimaryExpression {
        match self.peek() {
            Just(Token::Identifier(..)) => {
                let t = self.next().from_just().clone();
                match self.peek() {
                    Just(Token::OpenParen()) => {
                        PrimaryExpression::Call(t.clone(), self.parse_args())
                    }
                    _ => PrimaryExpression::Identifier(t.clone()),
                }
            }
            Just(Token::Integer(..)) => {
                PrimaryExpression::Integer(self.next().from_just().clone())
            }
            Just(Token::Float(..)) => PrimaryExpression::Float(self.next().from_just().clone()),
            Just(Token::If(..)) => PrimaryExpression::ConditionalExpression(self.parse_if_expr()),
            t => unexpected_token!(t, self),
        }
    }

    fn parse_if_expr(&mut self) -> ConditionalExpression {
        // if expr block [elif expr block ]* [else block]
        read_token!(self, Token::If);
        let mut choices = Vec::new();
        let expr = self.parse_expression();
        let block = self.parse_block();
        let mut or_else = Nothing;
        choices.push((Box::new(expr), Box::new(block)));
        loop {
            match self.peek() {
                Just(Token::Elif(..)) => {
                    read_token!(self, Token::Elif);
                    let expr = self.parse_expression();
                    let block = self.parse_block();
                    choices.push((Box::new(expr), Box::new(block)));
                }
                Just(Token::Else(..)) => {
                    read_token!(self, Token::Else);
                    let block = self.parse_block();
                    or_else = Just(Box::new(block));
                    break;
                }
                _ => break,
            }
        }
        ConditionalExpression::Exprs(choices, or_else)
    }

    fn parse_block(&mut self) -> Block {
        read_token!(self, Token::OpenBrace);
        let mut stmts = Vec::new();
        loop {
            match self.peek() {
                Just(Token::CloseBrace(..)) => {
                    break;
                }
                Just(_) => {
                    stmts.push(Box::new(self.parse_stmt()));
                }
                t => unexpected_token!(t, self),
            }
        }
        read_token!(self, Token::CloseBrace);
        Block::Statements(stmts)
    }

    fn parse_stmt(&mut self) -> Statement {
        let res = match_first!(
            self.peek() => _t,
            default unexpected_token!(_t, self),

            Declaration => Statement::Declaration(self.parse_declaration()),
            Expression => Statement::Expression(self.parse_expression()),
        );
        read_token!(self, Token::SemiColon);
        res
    }

    fn parse_args(&mut self) -> ArgList {
        let mut args = Vec::new();

        read_token!(self, Token::OpenParen);
        loop {
            match self.peek() {
                Just(Token::CloseParen()) => {
                    self.next();
                    break;
                }
                Just(_) => {
                    args.push(self.parse_expression());
                    match self.peek() {
                        Just(Token::CloseParen()) => {
                            self.next();
                            break;
                        }
                        Just(Token::Comma()) => {
                            self.next();
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
                let op_s: &str = if let Token::Operator(s) = op {
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
                    "**" => eval_tree(&*list[0]).pow(eval_tree(&*list[1]) as u32),
                    "?" => {
                        if let Expression::Op(Token::Operator(op), ExprList::List(res_list)) =
                            &*list[1]
                        {
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
                    _ => unreachable!(),
                }
            }
            Expression::Unary(op, ExprList::List(list)) => {
                let op_s: &str = if let Token::Operator(s) = op {
                    &*s
                } else {
                    unreachable!()
                };
                match op_s {
                    "-" => -eval_tree(&*list[0]),
                    "~" => !eval_tree(&*list[0]),
                    _ => unreachable!(),
                }
            }
            Expression::Prefix(op, expr) => {
                let op_s: &str = if let Token::Operator(s) = op {
                    &*s
                } else {
                    unreachable!()
                };
                match op_s {
                    "~" => !eval_tree(&*expr),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn check(expr: &str, expected: i128) {
        let unary_precedence = default_unary_ops();
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
            iter: &mut vec.iter().peekable(),
        };
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
        let s = parse(
            &mut vec![
                Token::Let(),
                Token::Identifier(From::from("bar")),
                Token::Operator(From::from("=")),
                Token::Identifier(From::from("foo")),
                Token::OpenParen(),
                Token::CloseParen(),
                Token::SemiColon(),
            ]
            .iter()
            .peekable(),
        );
        assert_eq!(
            Just(&s)
                .translation_unit()
                .leaf()
                .declaration()
                .identifier()
                .from_just(),
            &From::from("bar")
        );
        assert_eq!(
            Just(&s)
                .translation_unit()
                .leaf()
                .declaration()
                .expr()
                .expr()
                .from_just(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                Token::Identifier(From::from("foo")),
                ArgList::Args(vec![])
            ))
        );
    }

    #[test]
    fn parse_fn_one_arg() {
        let s = parse(
            &mut vec![
                Token::Let(),
                Token::Identifier(From::from("bar")),
                Token::Operator(From::from("=")),
                Token::Identifier(From::from("foo")),
                Token::OpenParen(),
                Token::Identifier(From::from("asd")),
                Token::CloseParen(),
                Token::SemiColon(),
            ]
            .iter()
            .peekable(),
        );
        assert_eq!(
            Just(&s)
                .translation_unit()
                .leaf()
                .declaration()
                .identifier()
                .from_just(),
            &From::from("bar")
        );
        assert_eq!(
            Just(&s)
                .translation_unit()
                .leaf()
                .declaration()
                .expr()
                .expr()
                .from_just(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                Token::Identifier(From::from("foo")),
                ArgList::Args(vec![Expression::PrimaryExpression(
                    PrimaryExpression::Identifier(Token::Identifier(From::from("asd")))
                )])
            ))
        );
    }

    #[test]
    fn parse_fn_two_args() {
        let s = parse(
            &mut vec![
                Token::Let(),
                Token::Identifier(From::from("bar")),
                Token::Operator(From::from("=")),
                Token::Identifier(From::from("foo")),
                Token::OpenParen(),
                Token::Identifier(From::from("asd")),
                Token::Comma(),
                Token::Identifier(From::from("qwe")),
                Token::CloseParen(),
                Token::SemiColon(),
            ]
            .iter()
            .peekable(),
        );
        assert_eq!(
            Just(&s)
                .translation_unit()
                .leaf()
                .declaration()
                .identifier()
                .from_just(),
            &From::from("bar")
        );
        assert_eq!(
            Just(&s)
                .translation_unit()
                .leaf()
                .declaration()
                .expr()
                .expr()
                .from_just(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                Token::Identifier(From::from("foo")),
                ArgList::Args(vec![
                    Expression::PrimaryExpression(PrimaryExpression::Identifier(
                        Token::Identifier(From::from("asd"))
                    )),
                    Expression::PrimaryExpression(PrimaryExpression::Identifier(
                        Token::Identifier(From::from("qwe"))
                    ))
                ])
            ))
        );
    }

    #[test]
    fn parse_fn_three_args() {
        let s = parse(
            &mut vec![
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
                Token::SemiColon(),
            ]
            .iter()
            .peekable(),
        );
        assert_eq!(
            Just(&s)
                .translation_unit()
                .leaf()
                .declaration()
                .identifier()
                .from_just(),
            &From::from("bar")
        );
        assert_eq!(
            Just(&s)
                .translation_unit()
                .leaf()
                .declaration()
                .expr()
                .expr()
                .from_just(),
            &Expression::PrimaryExpression(PrimaryExpression::Call(
                Token::Identifier(From::from("foo")),
                ArgList::Args(vec![
                    Expression::PrimaryExpression(PrimaryExpression::Identifier(
                        Token::Identifier(From::from("asd"))
                    )),
                    Expression::PrimaryExpression(PrimaryExpression::Identifier(
                        Token::Identifier(From::from("qwe"))
                    )),
                    Expression::PrimaryExpression(PrimaryExpression::Identifier(
                        Token::Identifier(From::from("aoeu"))
                    ))
                ])
            ))
        );
    }
}
