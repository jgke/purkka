#![recursion_limit = "100"]
#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![plugin(purkkasyntax_procmacros)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

use std::collections::HashMap;

use std::rc::Rc;

use std::ops::{Add, Div};

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
       -> &Declaration
        | &OperatorOverload
        | &ImportFile
        | &Typedef
        ;

    OperatorOverload
       -> #Token::NewOperator #Token::StringLiteral #Token::Operator Function
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum OperatorOverload { OperatorOverload(Rc<str>, Box<TypeSignature>, Box<Expression>) }
        ;

    Declaration
       -> Declaration. Visibility Mutability #Token::Identifier MaybeType #Token::SemiColon
        | Definition. Visibility Mutability #Token::Identifier MaybeType #Token::Operator Expression #Token::SemiColon
        | Function. Visibility #Token::Fun #Token::Identifier Function
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Declaration { Declaration(bool, bool, Rc<str>, Box<TypeSignature>, Option<Box<Expression>>) }
        ;

    Function
       -> &ParamList #Token::Operator TypeSignature &FunctionBody
        | Infer. &ParamList &FunctionBody
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

    LambdaParam
       -> #Token::Identifier #Token::Operator TypeSignature
        | #Token::Identifier
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum LambdaParam {
            LambdaParam(Rc<str>, Box<TypeSignature>),
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
        | Call. PrimaryExpression ArgList
        | Literal
        | StructInitialization. #Token::Identifier /* ident:typename */ #Token::OpenBrace InitializationFields #Token::CloseBrace
        | ArrayAccess. PrimaryExpression #Token::OpenBracket Expression #Token::CloseBracket
        | BlockExpression
        | Expression. #Token::OpenParen Expression #Token::CloseParen
        | Lambda
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum PrimaryExpression {
            Identifier(Rc<str>),
            StructInitialization(Rc<str>, Vec<StructInitializationField>),
            Literal(Literal),
            BlockExpression(Box<BlockExpression>),
            Expression(Box<Expression>),
            Lambda(Lambda),
        }
        ;

    InitializationFields
       -> Epsilon
        | StructInitializationField TrailingComma
        | More. StructInitializationField #Token::Comma InitializationFields
        ;

    StructInitializationField
       -> Expression
        | #Token::Identifier Token::Operator Expression
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum StructInitializationField {
            StructInitializationField(Option<Rc<str>>, Box<Expression>)
        }
        ;

    Lambda
       -> #Token::Fun ParamList #Token::Operator Block
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Lambda {
            Lambda(Vec<LambdaParam>, TypeSignature, BlockExpression),
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
            For(Option<Box<Statement>>, Option<Box<Expression>>, Option<Box<Expression>>, Box<Block>, Option<Box<Block>>),
        }
        ;

    ConditionalExpression
       -> #Token::If &Expression Block IfTail
        ;

    WhileExpression
       -> #Token::While &Expression Block IfTail
        ;

    ForExpression
       -> #Token::For #Token::OpenParen &ForConditions #Token::CloseParen Block &IfTail
        ;

    ForConditions
       -> MaybeExpression #Token::SemiColon MaybeExpression #Token::SemiColon MaybeExpression
        ;

    IfTail
       -> Epsilon
        | #Token::Elif &Expression Block IfTail
        | #Token::Else &Expression Block
        ;

    Expression
       -> PrimaryExpression
        | Op. #Token::Operator ExprList
        | Unary. #Token::Operator ExprList
        | PostFix. Expression #Token::Operator
        | Cast. Expression #Token::As TypeSignature
        | Call. &PrimaryExpression ArgList
        | ArrayAccess. &PrimaryExpression #Token::OpenBracket &Expression #Token::CloseBracket
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Expression {
            PrimaryExpression(PrimaryExpression),
            Op(Rc<str>, ExprList),
            Unary(Rc<str>, ExprList),
            PostFix(Box<Expression>, Rc<str>),
            Cast(Box<Expression>, TypeSignature),
            Call(Box<Expression>, ArgList),
            ArrayAccess(Box<Expression>, Box<Expression>),
        }
        ;

    ExprList -> Expression | Expression ExprList
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum ExprList { List(Vec<Expression>) }
        ;

    Block -> #Token::OpenBrace Statements #Token::CloseBrace
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Block { Statements(Vec<Statement>) }
        ;

    Statements -> Epsilon | &Statement #Token::SemiColon Statements;
    Statement
       -> Declaration #Token::SemiColon
        | BlockExpression
        | Expression #Token::SemiColon
        | ReturnStatement #Token::SemiColon
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Statement {
            Declaration(Box<Declaration>),
            BlockExpression(Box<BlockExpression>),
            Expression(Box<Expression>),
            Return(Option<Box<Expression>>),
        }
        ;

    ReturnStatement
       -> #Token::Return MaybeExpression
        ;

    Path -> #Token::Identifier;

    TrailingComma -> #Token::Comma | Epsilon;
    MaybeIdentifier -> #Token::Identifier | Epsilon;
    MaybeExpression -> &Expression | Epsilon;
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
impl_enter_unbox!(Declaration, Declaration, TypeSignature, ty, 4);
impl_enter_unbox_fmap!(Declaration, Declaration, Expression, expr, 5);
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
    Tuple(Vec<TypeSignature>),
    Array(Box<TypeSignature>, Option<usize>),
    DynamicArray(Box<TypeSignature>, Box<Expression>),

    Function(Vec<Param>, Box<TypeSignature>),
    Infer(IntermediateType),
}

impl From<TypeSignature> for IntermediateType {
    fn from(ty: TypeSignature) -> Self {
        match ty {
            TypeSignature::Infer(intermediate) => intermediate,
            otherwise => IntermediateType::Exact(Box::new(otherwise)),
        }
    }
}

impl From<IntermediateType> for TypeSignature {
    fn from(ty: IntermediateType) -> Self {
        match ty {
            IntermediateType::Exact(ty) => *ty,
            otherwise => TypeSignature::Infer(otherwise),
        }
    }
}

impl From<LambdaParam> for Param {
    fn from(LambdaParam::LambdaParam(ident, ty): LambdaParam) -> Self {
        Param::Param(ident, ty)
    }
}

impl From<Param> for LambdaParam {
    fn from(param: Param) -> Self {
        match param {
            Param::TypeOnly(ty) => LambdaParam::LambdaParam(From::from(""), ty),
            Param::Param(ident, ty) => LambdaParam::LambdaParam(ident, ty),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum IntermediateType {
    Exact(Box<TypeSignature>),
    Any(i128),
    Number(i128, IntermediateNumber),
}

#[derive(Clone, Debug, PartialEq)]
pub enum IntermediateNumber {
    Float,
    Double,
    Integer(Option<usize>, Option<usize>),
    Indeterminate,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Param {
    TypeOnly(Box<TypeSignature>),
    Param(Rc<str>, Box<TypeSignature>),
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

impl From<Param> for TypeSignature {
    fn from(param: Param) -> Self {
        match param {
            Param::Param(name, box TypeSignature::Infer(..)) => TypeSignature::Plain(name),
            Param::Param(_name, ty) => *ty,
            Param::TypeOnly(ty) => *ty,
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
            Some(box Expression::PrimaryExpression(PrimaryExpression::Lambda(..))),
        ) = self
        {
            true
        } else {
            false
        }
    }
}

impl Add for Literal {
    type Output = Literal;

    fn add(self, other: Literal) -> Literal {
        match (self, other) {
            (
                Literal::Integer(Token::Integer(i, left)),
                Literal::Integer(Token::Integer(_, right)),
            ) => Literal::Integer(Token::Integer(i, left + right)),
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }
}

impl Div for Literal {
    type Output = Literal;

    fn div(self, other: Literal) -> Literal {
        match (self, other) {
            (
                Literal::Integer(Token::Integer(i, left)),
                Literal::Integer(Token::Integer(_, right)),
            ) => Literal::Integer(Token::Integer(i, left / right)),
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }
}

impl Expression {
    pub fn eval(&self, constants: &HashMap<Rc<str>, Literal>) -> Result<Literal, Rc<str>> {
        match self {
            Expression::PrimaryExpression(e) => e.eval(constants),
            Expression::Op(op, ExprList::List(list)) => match op.as_ref() {
                "+" => Ok(list[0].eval(constants)? + list[1].eval(constants)?),
                "/" => Ok(list[0].eval(constants)? / list[1].eval(constants)?),
                otherwise => panic!("Not implemented: {:?}", otherwise),
            },
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }
}

impl PrimaryExpression {
    pub fn eval(&self, constants: &HashMap<Rc<str>, Literal>) -> Result<Literal, Rc<str>> {
        match self {
            PrimaryExpression::Literal(lit @ Literal::Integer(Token::Integer(..))) => {
                Ok(lit.clone())
            }
            PrimaryExpression::Identifier(s) => constants.get(s).cloned().ok_or_else(|| s.clone()),
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }
}

impl From<&str> for PrimaryExpression {
    fn from(ty: &str) -> Self {
        PrimaryExpression::Identifier(From::from(ty))
    }
}

impl From<&str> for Box<PrimaryExpression> {
    fn from(ty: &str) -> Self {
        Box::new(PrimaryExpression::Identifier(From::from(ty)))
    }
}
