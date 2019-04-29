#![recursion_limit = "100"]
#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![plugin(purkkasyntax_procmacros)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

use std::convert::TryFrom;
use std::rc::Rc;

use purkkatoken::token::Token;

grammar! {
    S -> TranslationUnit;

    TranslationUnit
       -> Leaf. Unit
        | List. Unit TranslationUnit
        | Epsilon
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum TranslationUnit { Units(Vec<Unit>) }
        ;

    Unit
       -> Declaration
        | OperatorOverload
        | ImportFile
        | Typedef
        ;

    OperatorOverload
       -> #Token::NewOperator #Token::StringLiteral #Token::Operator Function
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum OperatorOverload { OperatorOverload(Rc<str>, TypeSignature, Expression) }
        ;

    Declaration
       -> Declaration. Visibility Mutability #Token::Identifier MaybeType #Token::SemiColon
        | Definition. Visibility Mutability #Token::Identifier MaybeType #Token::Operator Expression #Token::SemiColon
        | Function. Visibility #Token::Fun #Token::Identifier Function
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Declaration { Declaration(bool, bool, Rc<str>, Option<TypeSignature>, Option<Expression>) }
        ;

    Function
       -> ParamList #Token::Operator TypeSignature FunctionBody
        | Infer. ParamList FunctionBody
        ;

    FunctionBody
       -> Block
        | Expression #Token::SemiColon
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
          /* [int], [int;5] */
        | Array. #Token::OpenBracket TypeSignature #Token::CloseBracket
        | SizedArray. #Token::OpenBracket TypeSignature #Token::SemiColon Expression #Token::CloseBracket
          /* *int, &int */
        | Pointer. #Token::Operator TypeSignature
          /* (foo, bar: int) -> int */
          /* int -> int */
        | Function. #Token::OpenParen ParamList #Token::CloseParen #Token::Operator TypeSignature
        | SingleParameterFunction. #Token::Identifier #Token::Operator TypeSignature
          /* (int, int) */
        | #Token::OpenParen TupleList #Token::CloseParen
        @ pub type _TypeSignature = TypeSignature
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
        @ pub type _Param = Param
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
        @ pub type _StructField = StructField
        ;

    EnumField
       -> #Token::Identifier
        | #Token::Identifier #Token::Operator TypeSignature
        @ pub type _EnumField = EnumField
        ;

    Mutability
       -> Mutable. #Token::Let
        | Const. #Token::Const
        ;

    Visibility
       -> Public. #Token::Pub
        | Private. Epsilon
        ;

    ImportFile
       -> Normal. #Token::Import Path
        | FFI. #Token::Import #Token::OpenParen #Token::Identifier #Token::CloseParen Path
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum ImportFile {
            Import(Rc<str>, Option<Rc<str>>)
        }
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

    Literal
       -> #Token::Integer
        | #Token::Float
        | #Token::StringLiteral
        ;

    PrimaryExpression
       -> #Token::Identifier
        | Call. #Token::Identifier ArgList
        | Literal
        | ArrayAccess. PrimaryExpression #Token::OpenBracket Expression #Token::CloseBracket
        | BlockExpression
        | Expression. #Token::OpenParen Expression #Token::CloseParen
        | Lambda
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum PrimaryExpression {
            Identifier(Rc<str>),
            Call(Rc<str>, ArgList),
            Literal(Literal),
            BlockExpression(Box<BlockExpression>),
            Expression(Box<Expression>),
            ArrayAccess(Box<PrimaryExpression>, Box<Expression>),
            Lambda(Lambda),
        }
        ;

    Lambda
        -> #Token::Fun ParamList #Token::Operator Block
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Lambda {
            Lambda(Vec<Param>, TypeSignature, BlockExpression),
        }
        ;

    BlockExpression
       -> Block
        | ConditionalExpression
        | WhileExpression
        | ForExpression
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum BlockExpression {
            Block(Block),
            If(Vec<(Box<Expression>, Box<Block>)>, Option<Box<Block>>),
            While(Box<Expression>, Box<Block>, Option<Box<Block>>),
            For(Option<Box<Statement>>, Option<Box<Statement>>, Option<Box<Statement>>, Box<Block>, Option<Box<Block>>),
        }
        ;

    ConditionalExpression
       -> #Token::If Expression Block IfTail
        ;

    WhileExpression
       -> #Token::While Expression Block IfTail
        ;

    ForExpression
       -> #Token::For #Token::OpenParen ForConditions #Token::CloseParen Block IfTail
        ;

    ForConditions
       -> MaybeExpression #Token::SemiColon MaybeExpression #Token::SemiColon MaybeExpression
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
        | PostFix. Expression #Token::Operator
        ;

    ExprList -> Expression | Expression ExprList
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum ExprList { List(Vec<Box<Expression>>) }
        ;

    Block -> #Token::OpenBrace Statements #Token::CloseBrace
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Block { Statements(Vec<Box<Statement>>) }
        ;

    Statements -> Epsilon | Statement #Token::SemiColon Statements;
    Statement
       -> Declaration #Token::SemiColon
        | BlockExpression
        | Expression #Token::SemiColon
        | ReturnStatement #Token::SemiColon
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Statement {
            Declaration(Declaration),
            BlockExpression(BlockExpression),
            Expression(Expression),
            Return(Option<Expression>),
        }
        ;

    ReturnStatement
       -> #Token::Return MaybeExpression
        ;

    Path -> #Token::Identifier;

    TrailingComma -> #Token::Comma | Epsilon;
    MaybeIdentifier -> #Token::Identifier | Epsilon;
    MaybeExpression -> Expression | Epsilon;
}

/* These macros create traits and functions for Option<T> -> Option<U> eg. the first one creates
 * translation_unit :: Option<S> -> Option<TranslationUnit> by matching on the first field in
 * S::TranslationUnit. The macro is defined in purkkaparser_procmacros. The essential benefit is
 * the safe chaining of methods like
 * Some(s).translation_unit().units().map(|t|t[0]).declaration()... These are a bit hard to create
 * in the grammar, since half of these types are handwritten. */
impl_enter!(S, TranslationUnit, TranslationUnit, translation_unit, 1);
impl_enter!(TranslationUnit, Units, "Vec<Unit>", units, 1);
impl_enter!(Unit, Declaration, Declaration, declaration, 1);
impl_enter_fmap!(Declaration, Declaration, TypeSignature, ty, 4);
impl_enter_fmap!(Declaration, Declaration, Expression, expr, 5);
impl_enter!(Declaration, Declaration, "Rc<str>", identifier, 3);

impl_enter!(Token, Identifier, "Rc<str>", identifier_s, 2);

#[derive(Clone, Debug, PartialEq)]
pub enum TypeSignature {
    Plain(Rc<str>),
    Pointer {
        nullable: bool,
        ty: Box<TypeSignature>,
    },

    Struct(Option<Rc<str>>, Vec<StructField>),
    Enum(Option<Rc<str>>, Vec<EnumField>),
    Tuple(Vec<Box<TypeSignature>>),
    Array(Box<TypeSignature>, Option<usize>),
    DynamicArray(Box<TypeSignature>, Option<Box<Expression>>),

    Function(Vec<Param>, Box<TypeSignature>),
    Infer,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Param {
    Param(Rc<str>, Box<TypeSignature>),
    Anon(Box<TypeSignature>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructField {
    Field {
        name: Rc<str>,
        ty: Box<TypeSignature>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum EnumField {
    Field {
        name: Rc<str>,
        value: Option<i128>,
        ty: Option<TypeSignature>,
    },
}

impl TryFrom<Param> for TypeSignature {
    type Error = ();

    fn try_from(param: Param) -> Result<Self, Self::Error> {
        match param {
            Param::Anon(ty) => Ok(*ty),
            Param::Param(..) => Err(()),
        }
    }
}

impl Declaration {
    pub fn is_fn(&self) -> bool {
        if let Declaration::Declaration(
            _,
            false,
            _,
            _,
            Some(Expression::PrimaryExpression(PrimaryExpression::Lambda(
                ..
            ))),
        ) = self
        {
            true
        } else {
            false
        }
    }
}
