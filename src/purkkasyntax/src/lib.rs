#![recursion_limit = "100"]
#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![plugin(purkkasyntax_procmacros)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![feature(associated_type_defaults)]

use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, Ordering};

use std::rc::Rc;

use std::ops::{Add, Div, Mul, Sub};

use purkkatoken::token::Token;

pub mod visitor;

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
        | Function. Visibility Inline_ #Token::Fun #Token::Identifier Function
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Declaration { Declaration(DeclarationFlags, Rc<str>, Box<TypeSignature>, Option<Box<Expression>>) }
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
       -> #Token::Type #Token::Identifier TypeSignature #Token::SemiColon
        | #Token::Struct #Token::Identifier #Token::OpenBrace StructFieldList #Token::CloseBrace
        | #Token::Enum #Token::Identifier #Token::OpenBrace EnumFieldList #Token::CloseBrace
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Typedef {
            Alias(bool, Rc<str>, Box<TypeSignature>),
            Struct(Rc<str>, Vec<StructField>),
            Enum(Rc<str>, Vec<EnumField>),
        }
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
            Variadic
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

    Inline_
       -> NoInline. Epsilon
        | Inline. #Token::Inline
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
        ;

    ArrayLiteral
       -> Empty. #Token::OpenBracket #Token::CloseBracket
        | Args. #Token::OpenBracket Arg MoreArgs #Token::CloseBracket
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
        | #Token::Char
        ;

    PrimaryExpression
       -> #Token::Identifier
        | Literal
        | StructInitialization. #Token::Identifier /* ident:typename & struct */ #Token::OpenBrace InitializationFields #Token::CloseBrace
        | VectorInitialization. #Token::Identifier /* ident:typename & vector */ #Token::OpenBrace InitializationFields #Token::CloseBrace
        | ArrayLiteral
        | BlockExpression
        | Expression. #Token::OpenParen Expression #Token::CloseParen
        | Lambda
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum PrimaryExpression {
            Identifier(Rc<str>),
            StructInitialization(Rc<str>, Vec<StructInitializationField>),
            VectorInitialization(Rc<str>, Vec<Expression>),
            ArrayLiteral(Vec<Expression>),
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
            Lambda(Vec<LambdaParam>, TypeSignature, Block),
        }
        ;

    BlockExpression
       -> Block
        | ConditionalExpression
        | WhileExpression
        | DoWhileExpression
        | ForExpression
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum BlockExpression {
            Block(Block),
            If(Vec<(Box<Expression>, Box<Block>)>, Option<Box<Block>>),
            While(Box<Expression>, Box<Block>, Option<Box<Block>>, bool),
            For(Option<Box<Statement>>, Option<Box<Expression>>, Option<Box<Expression>>, Box<Block>, Option<Box<Block>>),
        }
        ;

    ConditionalExpression
       -> #Token::If &Expression Block IfTail
        ;

    DoWhileExpression
       -> #Token::Do Block #Token::While &Expression
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
        | StructAccess. PrimaryExpression #Token::Dot #Token::Identifier
        | SizeofExpression. #Token::Sizeof #Token::OpenParen &Expression #Token::CloseParen
        | SizeofType. #Token::Sizeof #Token::OpenParen &TypeSignature #Token::CloseParen
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Expression {
            PrimaryExpression(PrimaryExpression),
            Op(Rc<str>, ExprList),
            Unary(Rc<str>, ExprList),
            PostFix(Box<Expression>, Rc<str>),
            Cast(Box<Expression>, TypeSignature),
            Call(Box<Expression>, Vec<Expression>),
            ArrayAccess(Box<Expression>, Box<Expression>),
            StructAccess(Box<Expression>, Rc<str>),
            Sizeof(Sizeof),
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
        | JumpStatement #Token::SemiColon
        | PragmaStatement
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Statement {
            Declaration(Box<Declaration>),
            BlockExpression(Box<BlockExpression>),
            Expression(Box<Expression>),
            Return(Option<Box<Expression>>),
            Jump(JumpStatement),
            Pragma(Rc<str>),
        }
        ;

    PragmaStatement
       -> #Token::Pragma #Token::StringLiteral
        ;

    ReturnStatement
       -> #Token::Return MaybeExpression
        ;

    JumpStatement
       -> #Token::Break
        | #Token::Continue
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum JumpStatement {
            Break,
            Continue,
        }
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
impl_enter_unbox!(Declaration, Declaration, TypeSignature, ty, 3);
impl_enter_unbox_fmap!(Declaration, Declaration, Expression, expr, 4);
impl_enter!(Declaration, Declaration, "Rc<str>", identifier, 2);

impl_enter!(Token, Identifier, "Rc<str>", identifier_s, 2);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct DeclarationFlags {
    pub mutable: bool,
    pub public: bool,
    pub inline: bool,
    pub static_: bool,
}

impl Default for DeclarationFlags {
    fn default() -> DeclarationFlags {
        DeclarationFlags {
            mutable: true,
            public: true,
            inline: false,
            static_: false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeSignature {
    Plain(Rc<str>),
    Primitive(Primitive),
    Vector(Primitive),
    Complex(Primitive),
    Pointer {
        nullable: bool,
        ty: Box<TypeSignature>,
    },

    Struct(Option<Rc<str>>, Vec<StructField>),
    Enum(Option<Rc<str>>, Vec<EnumField>),
    Union(Option<Rc<str>>, Vec<StructField>),
    Tuple(Vec<TypeSignature>),
    Array(Box<TypeSignature>, Option<usize>),
    DynamicArray(Box<TypeSignature>, Box<Expression>),

    Function(Vec<Param>, Box<TypeSignature>),
    Infer(IntermediateType),
    Attribute(Box<TypeSignature>, Vec<Attribute>)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Attribute {
    Vector(Expression),
    Other(Vec<Token>)
}

impl TypeSignature {
    pub fn is_ptr(&self, context: &HashMap<i128, IntermediateType>) -> bool {
        use TypeSignature::*;
        match self {
            Pointer { .. } | Array(..) | DynamicArray(..) => true,
            Infer(infer) => infer.is_ptr(context),
            _ => false,
        }
    }

    pub fn is_array(&self, context: &HashMap<i128, IntermediateType>) -> Option<Box<TypeSignature>> {
        use TypeSignature::*;
        match self {
            Array(ty, _) | DynamicArray(ty, _) => Some(ty.clone()),
            Infer(infer) => infer.is_array(context),
            _ => None,
        }
    }

    pub fn is_compound(&self, context: &HashMap<i128, IntermediateType>) -> bool {
        use TypeSignature::*;
        match self {
            Struct(..) | Enum(..) | Union(..) => true,
            Infer(infer) => infer.is_compound(context),
            _ => false,
        }
    }

    pub fn dereference(&self, context: &HashMap<i128, IntermediateType>) -> Option<TypeSignature> {
        use TypeSignature::*;
        match self {
            Pointer { ty, .. } | Array(ty, _) | DynamicArray(ty, ..) => Some(*ty.clone()),
            Infer(infer) => infer.dereference(context),
            _ => None,
        }
    }

    pub fn access(
        &self,
        ident: &str,
        context: &HashMap<i128, IntermediateType>,
    ) -> (Option<i128>, Option<TypeSignature>) {
        match self {
            TypeSignature::Struct(_, fields) | TypeSignature::Union(_, fields) => {
                for StructField { name, ty, .. } in fields {
                    if name.as_ref() == ident {
                        return (None, Some(*ty.clone()));
                    }
                }
                (None, None)
            }
            TypeSignature::Enum(_, fields) => {
                for EnumField { name, value, .. } in fields {
                    if name.as_ref() == ident {
                        return (
                            Some(*value),
                            Some(TypeSignature::Primitive(Primitive::Int(32))),
                        );
                    }
                }
                (None, None)
            }
            TypeSignature::Infer(infer) => infer.access(ident, context),
            _ => (None, None),
        }
    }

    pub fn address_of(self) -> TypeSignature {
        TypeSignature::Pointer {
            ty: Box::new(self),
            nullable: false,
        }
    }

    pub fn void() -> TypeSignature {
        TypeSignature::Primitive(Primitive::Void)
    }

    pub fn char() -> TypeSignature {
        TypeSignature::Primitive(Primitive::Char)
    }

    pub fn int() -> TypeSignature {
        TypeSignature::Primitive(Primitive::Int(32))
    }

    pub fn long() -> TypeSignature {
        TypeSignature::Primitive(Primitive::Int(64))
    }

    pub fn size_t() -> TypeSignature {
        TypeSignature::Primitive(Primitive::UInt(64))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Primitive {
    Void,
    Char,
    Int(usize),
    UInt(usize),
    Float,
    Double,
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Primitive::Void => write!(f, "void"),
            Primitive::Char => write!(f, "char"),
            Primitive::Int(size) => write!(f, "i{}", size),
            Primitive::UInt(size) => write!(f, "u{}", size),
            Primitive::Float => write!(f, "float"),
            Primitive::Double => write!(f, "double"),
        }
    }
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
    fn from(param: LambdaParam) -> Self {
        match param {
            LambdaParam::LambdaParam(ident, ty) => Param::Param(ident, ty),
            LambdaParam::Variadic => Param::Variadic,
        }
    }
}

impl From<Param> for LambdaParam {
    fn from(param: Param) -> Self {
        match param {
            Param::TypeOnly(ty) => LambdaParam::LambdaParam(From::from(""), ty),
            Param::Param(ident, ty) => LambdaParam::LambdaParam(ident, ty),
            Param::Variadic => LambdaParam::Variadic,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum IntermediateType {
    Exact(Box<TypeSignature>),
    Any(i128),
    Number(i128, IntermediateNumber),
}

static TYPE_COUNTER: AtomicI64 = AtomicI64::new(1);

impl IntermediateType {
    pub fn new_any() -> IntermediateType {
        IntermediateType::Any(TYPE_COUNTER.fetch_add(1, Ordering::Relaxed).into())
    }

    pub fn new_number(num: IntermediateNumber) -> IntermediateType {
        IntermediateType::Number(TYPE_COUNTER.fetch_add(1, Ordering::Relaxed).into(), num)
    }

    pub fn generic_any() -> IntermediateType {
        IntermediateType::Any(0)
    }

    pub fn generic_number(num: IntermediateNumber) -> IntermediateType {
        IntermediateType::Number(0, num)
    }

    pub fn is_ptr(&self, context: &HashMap<i128, IntermediateType>) -> bool {
        match self {
            IntermediateType::Any(id) => {
                if let Some(ty) = context.get(id) {
                    ty.is_ptr(context)
                } else {
                    false
                }
            }
            IntermediateType::Number(..) => false,
            IntermediateType::Exact(t) => t.is_ptr(context),
        }
    }

    pub fn is_array(&self, context: &HashMap<i128, IntermediateType>) -> Option<Box<TypeSignature>> {
        match self {
            IntermediateType::Any(id) => {
                if let Some(ty) = context.get(id) {
                    ty.is_array(context)
                } else {
                    None
                }
            }
            IntermediateType::Number(..) => None,
            IntermediateType::Exact(t) => t.is_array(context),
        }
    }

    pub fn is_compound(&self, context: &HashMap<i128, IntermediateType>) -> bool {
        match self {
            IntermediateType::Any(id) => {
                if let Some(ty) = context.get(id) {
                    ty.is_compound(context)
                } else {
                    false
                }
            }
            IntermediateType::Number(..) => false,
            IntermediateType::Exact(t) => t.is_compound(context),
        }
    }

    pub fn dereference(&self, context: &HashMap<i128, IntermediateType>) -> Option<TypeSignature> {
        match self {
            IntermediateType::Any(id) => {
                if let Some(ty) = context.get(id) {
                    ty.dereference(context)
                } else {
                    None
                }
            }
            IntermediateType::Number(..) => None,
            IntermediateType::Exact(t) => t.dereference(context),
        }
    }

    pub fn access(
        &self,
        ident: &str,
        context: &HashMap<i128, IntermediateType>,
    ) -> (Option<i128>, Option<TypeSignature>) {
        match self {
            IntermediateType::Any(id) => {
                if let Some(ty) = context.get(id) {
                    ty.access(ident, context)
                } else {
                    (None, None)
                }
            }
            IntermediateType::Number(..) => (None, None),
            IntermediateType::Exact(t) => t.access(ident, context),
        }
    }

    pub fn address_of(self) -> IntermediateType {
        IntermediateType::Exact(Box::new(TypeSignature::Pointer {
            ty: Box::new(From::from(self)),
            nullable: false,
        }))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
    Variadic,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructField {
    pub name: Rc<str>,
    pub ty: Box<TypeSignature>,
    pub bitfield: Option<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumField {
    pub name: Rc<str>,
    pub value: i128,
    pub ty: Option<TypeSignature>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Sizeof {
    Expression(Box<Expression>),
    Type(Box<TypeSignature>),
}

impl Param {
    pub fn ty_field(&self) -> &TypeSignature {
        match self {
            Param::Param(_, ty) => &*ty,
            Param::TypeOnly(ty) => &*ty,
            Param::Variadic => unimplemented!(),
        }
    }

    pub fn set_ty(&mut self, new_ty: TypeSignature) {
        match self {
            Param::Param(_, ty) => **ty = new_ty,
            Param::TypeOnly(ty) => **ty = new_ty,
            Param::Variadic => unimplemented!(),
        }
    }
}

impl From<Param> for TypeSignature {
    fn from(param: Param) -> Self {
        match param {
            Param::Param(name, box TypeSignature::Infer(..)) => TypeSignature::Plain(name),
            Param::Param(_name, ty) => *ty,
            Param::TypeOnly(ty) => *ty,
            Param::Variadic => unimplemented!(),
        }
    }
}

impl Declaration {
    pub fn is_fn(&self) -> bool {
        if let Declaration::Declaration(
            flags,
            _name,
            _ty,
            Some(box Expression::PrimaryExpression(PrimaryExpression::Lambda(..))),
        ) = self
        {
            !flags.mutable
        } else {
            false
        }
    }

    pub fn is_fn_ty(&self) -> bool {
        if let Declaration::Declaration(_flags, _name, box TypeSignature::Function(..), Some(_)) =
            self
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

impl Mul for Literal {
    type Output = Literal;

    fn mul(self, other: Literal) -> Literal {
        match (self, other) {
            (
                Literal::Integer(Token::Integer(i, left)),
                Literal::Integer(Token::Integer(_, right)),
            ) => Literal::Integer(Token::Integer(i, left * right)),
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }
}

impl Sub for Literal {
    type Output = Literal;

    fn sub(self, other: Literal) -> Literal {
        match (self, other) {
            (
                Literal::Integer(Token::Integer(i, left)),
                Literal::Integer(Token::Integer(_, right)),
            ) => Literal::Integer(Token::Integer(i, left - right)),
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
    pub fn eval(&self, constants: &HashMap<Rc<str>, Literal>) -> Result<Literal, ()> {
        match self {
            Expression::PrimaryExpression(e) => e.eval(constants),
            Expression::Op(op, ExprList::List(list)) => match op.as_ref() {
                "+" => Ok(list[0].eval(constants)? + list[1].eval(constants)?),
                "-" => Ok(list[0].eval(constants)? - list[1].eval(constants)?),
                "*" => Ok(list[0].eval(constants)? * list[1].eval(constants)?),
                "/" => Ok(list[0].eval(constants)? / list[1].eval(constants)?),
                otherwise => panic!("Not implemented: {:?}", otherwise),
            },
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }
}

impl PrimaryExpression {
    pub fn eval(&self, constants: &HashMap<Rc<str>, Literal>) -> Result<Literal, ()> {
        match self {
            PrimaryExpression::Literal(lit @ Literal::Integer(Token::Integer(..))) => {
                Ok(lit.clone())
            }
            PrimaryExpression::Identifier(s) => constants.get(s).cloned().ok_or(()),
            PrimaryExpression::Expression(e) => e.eval(constants),
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

#[derive(Clone, Debug, PartialEq)]
pub enum MacroExpansion {
    Expression(Expression),
    Statement(Statement),
    Type(TypeSignature),
    Typedef(Typedef),
}
