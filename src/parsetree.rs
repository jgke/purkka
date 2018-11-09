#![allow(dead_code)]

use tokentype;
use tokentype::{Constant, Identifier, StringLiteral, UnaryOperator};

#[derive(Debug)]
pub enum PrimaryExpression<'a> {
    Identifier(Identifier),
    Constant(Constant),
    StringLiteral(StringLiteral),
    Expression(&'a Expression<'a>),
}

#[derive(Debug)]
pub enum PostfixExpression<'a> {
    PrimaryExpression(&'a PrimaryExpression<'a>),
    ArrayAccess(&'a PostfixExpression<'a>, &'a Expression<'a>),
    Call(&'a PostfixExpression<'a>, &'a [AssignmentExpression<'a>]),
    DotAccess(&'a PostfixExpression<'a>, Identifier),
    PtrAccess(&'a PostfixExpression<'a>, Identifier),
    PostIncrement(&'a PostfixExpression<'a>),
    PostDecrement(&'a PostfixExpression<'a>)
}

#[derive(Debug)]
pub enum ArgumentExpressionList<'a> {
    Arguments(&'a [AssignmentExpression<'a>]),
}

#[derive(Debug)]
pub enum UnaryExpression<'a> {
    PostfixExpression(&'a PostfixExpression<'a>),
    PreIncrement(&'a PostfixExpression<'a>),
    PreDecrement(&'a PostfixExpression<'a>),
    CastExpression(UnaryOperator, &'a CastExpression<'a>),
    SizeofExpression(&'a UnaryExpression<'a>),
    SizeofType(&'a TypeName<'a>),
}

#[derive(Debug)]
pub enum CastExpression<'a> {
    UnaryExpression(&'a UnaryExpression<'a>),
    Cast(&'a TypeName<'a>, &'a CastExpression<'a>)
}

#[derive(Debug)]
pub enum MultiplicativeExpression<'a> {
    CastExpression(CastExpression<'a>),
    Times(&'a MultiplicativeExpression<'a>, CastExpression<'a>),
    Divide(&'a MultiplicativeExpression<'a>, CastExpression<'a>),
    Mod(&'a MultiplicativeExpression<'a>, CastExpression<'a>),
}

#[derive(Debug)]
pub enum AdditiveExpression<'a> {
    MultiplicativeExpression(MultiplicativeExpression<'a>),
    Plus(&'a AdditiveExpression<'a>, MultiplicativeExpression<'a>),
    Minus(&'a AdditiveExpression<'a>, MultiplicativeExpression<'a>)
}

#[derive(Debug)]
pub enum ShiftExpression<'a> {
    AdditiveExpression(AdditiveExpression<'a>),
    ShiftLeft(&'a ShiftExpression<'a>, AdditiveExpression<'a>),
    ShiftRight(&'a ShiftExpression<'a>, AdditiveExpression<'a>)
}

#[derive(Debug)]
pub enum RelationalExpression<'a> {
    ShiftExpression(ShiftExpression<'a>),
    LessThan(&'a RelationalExpression<'a>, ShiftExpression<'a>),
    GreaterThan(&'a RelationalExpression<'a>, ShiftExpression<'a>),
    LessEqThan(&'a RelationalExpression<'a>, ShiftExpression<'a>),
    GreaterEqThan(&'a RelationalExpression<'a>, ShiftExpression<'a>)
}

#[derive(Debug)]
pub enum EqualityExpression<'a> {
    RelationalExpression(RelationalExpression<'a>),
    Equals(&'a EqualityExpression<'a>, RelationalExpression<'a>),
    NotEquals(&'a EqualityExpression<'a>, RelationalExpression<'a>)
}

#[derive(Debug)]
pub enum BitAndExpression<'a> {
    EqualityExpression(EqualityExpression<'a>),
    And(&'a BitAndExpression<'a>, EqualityExpression<'a>)
}

#[derive(Debug)]
pub enum XorExpression<'a> {
    BitAndExpression(BitAndExpression<'a>),
    Xor(&'a XorExpression<'a>, BitAndExpression<'a>)
}

#[derive(Debug)]
pub enum BitOrExpression<'a> {
    XorExpression(XorExpression<'a>),
    Or(&'a BitOrExpression<'a>, XorExpression<'a>)
}

#[derive(Debug)]
pub enum AndExpression<'a> {
    BitOrExpression(BitOrExpression<'a>),
    And(&'a AndExpression<'a>, BitOrExpression<'a>)
}

#[derive(Debug)]
pub enum OrExpression<'a> {
    AndExpression(AndExpression<'a>),
    Or(&'a OrExpression<'a>, AndExpression<'a>)
}

#[derive(Debug)]
pub enum TernaryExpression<'a> {
    OrExpression(OrExpression<'a>),
    Ternary(&'a OrExpression<'a>, &'a Expression<'a>, &'a TernaryExpression<'a>)
}

#[derive(Debug)]
pub enum ConstantExpression<'a> {
    TernaryExpression(TernaryExpression<'a>)
}


#[derive(Debug)]
pub enum AssignmentExpression<'a> {
    TernaryExpression(&'a TernaryExpression<'a>),
    Assignment(UnaryExpression<'a>, &'a AssignmentExpression<'a>)
}

#[derive(Debug)]
pub enum Expression<'a> {
    AssignmentExpression(&'a AssignmentExpression<'a>),
    Comma(&'a Expression<'a>, &'a AssignmentExpression<'a>)
}

#[derive(Debug)]
pub enum Declaration<'a> {
    Declaration(DeclarationSpecifiers<'a>),
    Initialization(DeclarationSpecifiers<'a>, &'a [InitDeclarator<'a>])
}

#[derive(Debug)]
pub enum DeclarationSpecifiers<'a> {
    StorageClass(tokentype::StorageClass),
    StorageClassMore(tokentype::StorageClass, &'a DeclarationSpecifiers<'a>),
    TypeSpecifier(tokentype::TypeSpecifier),
    TypeSpecifierMore(tokentype::TypeSpecifier, &'a DeclarationSpecifiers<'a>),
    ComplexTypeSpecifier(ComplexType<'a>),
    ComplexTypeSpecifierMore(ComplexType<'a>, &'a DeclarationSpecifiers<'a>),
    TypeQualifier(TypeQualifier),
    TypeQualifierMore(TypeQualifier, &'a DeclarationSpecifiers<'a>)
}

#[derive(Debug)]
pub enum InitDeclarator<'a> {
    Declarator(Declarator<'a>),
    Initializer(Declarator<'a>, Initializer<'a>)
}

#[derive(Debug)]
pub enum StorageClassSpecifier<'a> {
    Declarator(Declarator<'a>),
    Initializer(Declarator<'a>, Initializer<'a>)
}

#[derive(Debug)]
pub enum ComplexType<'a> {
    StructOrUnion(StructOrUnion, Identifier, &'a [StructDeclaration<'a>]),
    AnonymausStructOrUnion(StructOrUnion, &'a [StructDeclaration<'a>]),
    OpaqueStructOrUnion(StructOrUnion, Identifier),
    AnonymousEnum(&'a [Enumerator<'a>]),
}

#[derive(Debug)]
pub enum StructOrUnion {
    Struct, Union
}

#[derive(Debug)]
pub enum StructDeclaration<'a> {
    Declarator(Declarator<'a>),
    AnonBitField(ConstantExpression<'a>),
    BitField(Declarator<'a>, ConstantExpression<'a>)
}

#[derive(Debug)]
pub enum SpecifierQualifierList<'a> {
    TypeSpecifier(tokentype::TypeSpecifier),
    TypeSpecifierMore(tokentype::TypeSpecifier, DeclarationSpecifiers<'a>),
    ComplexTypeSpecifier(ComplexType<'a>),
    ComplexTypeSpecifierMore(ComplexType<'a>, DeclarationSpecifiers<'a>),
    TypeQualifier(TypeQualifier),
    TypeQualifierMore(TypeQualifier, DeclarationSpecifiers<'a>)
}

#[derive(Debug)]
pub enum Enumerator<'a> {
    Auto(Identifier),
    Initializer(Identifier, ConstantExpression<'a>)
}

#[derive(Debug)]
pub enum TypeQualifier {
    Const, Volatile
}

#[derive(Debug)]
pub enum Declarator<'a> {
    Pointer(Pointer<'a>, &'a DirectDeclarator<'a>),
    DirectDeclarator(&'a DirectDeclarator<'a>)
}

#[derive(Debug)]
pub enum DirectDeclarator<'a> {
    Identifier(Identifier),
    Porens(&'a Declarator<'a>),
    SizedArray(&'a DirectDeclarator<'a>, ConstantExpression<'a>),
    UnsizedArray(&'a DirectDeclarator<'a>),
    TypedFunction(&'a DirectDeclarator<'a>, ParameterTypeList<'a>),
    UntypedFunction(&'a DirectDeclarator<'a>, &'a [Identifier]),
    Function(&'a DirectDeclarator<'a>)
}

#[derive(Debug)]
pub enum Pointer<'a> {
    Pointer,
    TypeQualifierList(&'a [TypeQualifier], &'a Pointer<'a>)
}

#[derive(Debug)]
pub enum ParameterTypeList<'a> {
    Parameters(&'a [ParameterDeclaration<'a>]),
    Varargs(&'a [ParameterDeclaration<'a>])
}

#[derive(Debug)]
pub enum ParameterDeclaration<'a> {
    Declarator(DeclarationSpecifiers<'a>, Declarator<'a>),
    AbstractDeclarator(DeclarationSpecifiers<'a>, AbstractDeclarator<'a>),
    DeclarationSpecifiers(DeclarationSpecifiers<'a>),
}

#[derive(Debug)]
pub enum TypeName<'a> {
    SpecifierQualifierList(SpecifierQualifierList<'a>),
    AbstractDeclarator(SpecifierQualifierList<'a>, AbstractDeclarator<'a>)
}

#[derive(Debug)]
pub enum AbstractDeclarator<'a> {
    Pointer(Pointer<'a>),
    DirectAbstractDeclarator(&'a DirectAbstractDeclarator<'a>),
    PointerToDirectAbstractDeclarator(Pointer<'a>, &'a DirectAbstractDeclarator<'a>),
}

#[derive(Debug)]
pub enum DirectAbstractDeclarator<'a> {
    Parens(&'a AbstractDeclarator<'a>),
    UnsizedArray,
    SizedArray(ConstantExpression<'a>),
    UnsizedArrayMore(&'a DirectAbstractDeclarator<'a>),
    SizedArrayMore(&'a DirectAbstractDeclarator<'a>, ConstantExpression<'a>),
    EmptyParens(),
    ParameterList(ParameterTypeList<'a>),
    EmptyParensMore(),
    ParameterListMore(ParameterTypeList<'a>),
}

#[derive(Debug)]
pub enum Initializer<'a> {
    AssignmentExpression(AssignmentExpression<'a>),
    InitializerList(&'a [Initializer<'a>])
}

#[derive(Debug)]
pub enum Statement<'a> {
    LabeledStatement(&'a LabeledStatement<'a>),
    CompoundStatement(&'a [Declaration<'a>], &'a [Statement<'a>]),
    ExpressionStatement(ExpressionStatement<'a>),
    SelectionStatement(&'a SelectionStatement<'a>),
    IterationStatement(&'a IterationStatement<'a>),
    JumpStatement(JumpStatement<'a>)
}

#[derive(Debug)]
pub enum LabeledStatement<'a> {
    GotoLabel(Identifier, &'a Statement<'a>),
    Case(Identifier, &'a Statement<'a>),
    DefaultCase(&'a Statement<'a>),
}

#[derive(Debug)]
pub enum ExpressionStatement<'a> {
    Empty,
    Expression(Expression<'a>)
}

#[derive(Debug)]
pub enum SelectionStatement<'a> {
    If(Expression<'a>, &'a Statement<'a>),
    IfElse(Expression<'a>, &'a Statement<'a>, &'a Statement<'a>),
    Switch(Expression<'a>, &'a Statement<'a>)
}

#[derive(Debug)]
pub enum IterationStatement<'a> {
    While(Expression<'a>, Statement<'a>),
    Do(Statement<'a>, Expression<'a>),
    For(ExpressionStatement<'a>, ExpressionStatement<'a>, Statement<'a>),
    ForDo(ExpressionStatement<'a>, ExpressionStatement<'a>, Expression<'a>, Statement<'a>)
}

#[derive(Debug)]
pub enum JumpStatement<'a> {
    Goto(Identifier),
    Continue,
    Break,
    Return,
    ReturnExpression(Expression<'a>)
}

#[derive(Debug)]
pub enum TranslationUnit<'a> {
    ExternalDeclaration(&'a [ExternalDeclaration<'a>])
}

#[derive(Debug)]
pub enum ExternalDeclaration<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    Declaration(Declaration<'a>)
}

#[derive(Debug)]
pub enum FunctionDefinition<'a> {
    DeclarationSpecifiers(DeclarationSpecifiers<'a>, Declarator<'a>, &'a [Declaration<'a>], &'a [Declaration<'a>], &'a [Statement<'a>]),
    Declaration(Declarator<'a>, &'a [Declaration<'a>], &'a [Declaration<'a>], &'a [Statement<'a>])
}
