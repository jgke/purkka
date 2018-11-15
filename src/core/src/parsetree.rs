/*
#![allow(dead_code)]

use tokentype;
use tokentype::{Constant, Identifier, StringLiteral, UnaryOperator};

macro_rules! box_all {
    (@box_one $x:ident, { $( $x:ident ( $( $p:ident ),* ) ),* } ) => {
        #[derive(Debug)]
        pub enum $name {
            $(
                $x(
                    $p
                ),
            )*
        }
    };

    ( $name:ident, $($params:tt)* ) => {
        box_all!(@box_one $name, $($params)*);
    };
}

box_all!(PrimaryExpressionMacro, {
    StringLiteral(StringLiteral),
    Identifier([Identifier]),
    Constant(Constant, [Identifier]),
    Expression(Expression)
});

#[derive(Debug)]
enum Expression {}

#[derive(Debug)]
pub enum TranslationUnit<'a> {
    ExternalDeclaration(&'a [PrimaryExpressionMacro])
}

/*

box_all!(PrimaryExpression,
    Identifier(Identifier),
    Constant(Constant),
    StringLiteral(StringLiteral),
    Expression(Expression)
);

box_all!(PostfixExpression,
    PrimaryExpression(PrimaryExpression),
    ArrayAccess(PostfixExpression, Expression),
    Call(PostfixExpression, [AssignmentExpression]),
    DotAccess(PostfixExpression, Identifier),
    PtrAccess(PostfixExpression, Identifier),
    PostIncrement(PostfixExpression),
    PostDecrement(PostfixExpression)
);

box_all!(ArgumentExpressionList,
    Arguments([AssignmentExpression]),
);

box_all!(UnaryExpression,
    PostfixExpression(PostfixExpression),
    PreIncrement(PostfixExpression),
    PreDecrement(PostfixExpression),
    CastExpression(UnaryOperator, CastExpression),
    SizeofExpression(UnaryExpression),
    SizeofType(TypeName),
);

box_all!(CastExpression,
    UnaryExpression(UnaryExpression),
    Cast(TypeName, CastExpression)
);

box_all!(MultiplicativeExpression,
    CastExpression(CastExpression),
    Times(MultiplicativeExpression, CastExpression),
    Divide(MultiplicativeExpression, CastExpression),
    Mod(MultiplicativeExpression, CastExpression),
);

box_all!(AdditiveExpression,
    MultiplicativeExpression(MultiplicativeExpression),
    Plus(AdditiveExpression, MultiplicativeExpression),
    Minus(AdditiveExpression, MultiplicativeExpression)
);

box_all!(ShiftExpression,
    AdditiveExpression(AdditiveExpression),
    ShiftLeft(ShiftExpression, AdditiveExpression),
    ShiftRight(ShiftExpression, AdditiveExpression)
);

box_all!(RelationalExpression,
    ShiftExpression(ShiftExpression),
    LessThan(RelationalExpression, ShiftExpression),
    GreaterThan(RelationalExpression, ShiftExpression),
    LessEqThan(RelationalExpression, ShiftExpression),
    GreaterEqThan(RelationalExpression, ShiftExpression)
);

box_all!(EqualityExpression,
    RelationalExpression(RelationalExpression),
    Equals(EqualityExpression, RelationalExpression),
    NotEquals(EqualityExpression, RelationalExpression)
);

box_all!(BitAndExpression,
    EqualityExpression(EqualityExpression),
    And(BitAndExpression, EqualityExpression)
);

box_all!(XorExpression,
    BitAndExpression(BitAndExpression),
    Xor(XorExpression, BitAndExpression)
);

box_all!(BitOrExpression,
    XorExpression(XorExpression),
    Or(BitOrExpression, XorExpression)
);

box_all!(AndExpression,
    BitOrExpression(BitOrExpression),
    And(AndExpression, BitOrExpression)
);

box_all!(OrExpression,
    AndExpression(AndExpression),
    Or(OrExpression, AndExpression)
);

box_all!(TernaryExpression,
    OrExpression(OrExpression),
    Ternary(OrExpression, Expression, TernaryExpression)
);

box_all!(ConstantExpression,
    TernaryExpression(TernaryExpression)
);


box_all!(AssignmentExpression,
    TernaryExpression(TernaryExpression),
    Assignment(UnaryExpression, AssignmentExpression)
);

box_all!(Expression,
    AssignmentExpression(AssignmentExpression),
    Comma(Expression, AssignmentExpression)
);

box_all!(Declaration,
    Declaration(DeclarationSpecifiers),
    Initialization(DeclarationSpecifiers, [InitDeclarator])
);

box_all!(DeclarationSpecifiers,
    StorageClass(tokentype::StorageClass),
    StorageClassMore(tokentype::StorageClass, DeclarationSpecifiers),
    TypeSpecifier(tokentype::TypeSpecifier),
    TypeSpecifierMore(tokentype::TypeSpecifier, DeclarationSpecifiers),
    ComplexTypeSpecifier(ComplexType),
    ComplexTypeSpecifierMore(ComplexType, DeclarationSpecifiers),
    TypeQualifier(TypeQualifier),
    TypeQualifierMore(TypeQualifier, DeclarationSpecifiers)
);

box_all!(InitDeclarator,
    Declarator(Declarator),
    Initializer(Declarator, Initializer)
);

box_all!(StorageClassSpecifier,
    Declarator(Declarator),
    Initializer(Declarator, Initializer)
);

box_all!(ComplexType,
    StructOrUnion(StructOrUnion, Identifier, [StructDeclaration]),
    AnonymausStructOrUnion(StructOrUnion, [StructDeclaration]),
    OpaqueStructOrUnion(StructOrUnion, Identifier),
    AnonymousEnum([Enumerator]),
);

box_all!(StructOrUnion,
    Struct(), Union()
);

box_all!(StructDeclaration,
    Declarator(Declarator),
    AnonBitField(ConstantExpression),
    BitField(Declarator, ConstantExpression)
);

box_all!(SpecifierQualifierList,
    TypeSpecifier(tokentype::TypeSpecifier),
    TypeSpecifierMore(tokentype::TypeSpecifier, DeclarationSpecifiers),
    ComplexTypeSpecifier(ComplexType),
    ComplexTypeSpecifierMore(ComplexType, DeclarationSpecifiers),
    TypeQualifier(TypeQualifier),
    TypeQualifierMore(TypeQualifier, DeclarationSpecifiers)
);

box_all!(Enumerator,
    Auto(Identifier),
    Initializer(Identifier, ConstantExpression)
);

box_all!(TypeQualifier,
    Const(), Volatile()
);

box_all!(Declarator,
    Pointer(Pointer, DirectDeclarator),
    DirectDeclarator(DirectDeclarator)
);

box_all!(DirectDeclarator,
    Identifier(Identifier),
    Porens(Declarator),
    SizedArray(DirectDeclarator, ConstantExpression),
    UnsizedArray(DirectDeclarator),
    TypedFunction(DirectDeclarator, ParameterTypeList),
    UntypedFunction(DirectDeclarator, [Identifier]),
    Function(DirectDeclarator)
);

box_all!(Pointer,
    Pointer(),
    TypeQualifierList([TypeQualifier], Pointer)
);

box_all!(ParameterTypeList,
    Parameters([ParameterDeclaration]),
    Varargs([ParameterDeclaration])
);

box_all!(ParameterDeclaration,
    Declarator(DeclarationSpecifiers, Declarator),
    AbstractDeclarator(DeclarationSpecifiers, AbstractDeclarator),
    DeclarationSpecifiers(DeclarationSpecifiers),
);

box_all!(TypeName,
    SpecifierQualifierList(SpecifierQualifierList),
    AbstractDeclarator(SpecifierQualifierList, AbstractDeclarator)
);

box_all!(AbstractDeclarator,
    Pointer(Pointer),
    DirectAbstractDeclarator(DirectAbstractDeclarator),
    PointerToDirectAbstractDeclarator(Pointer, DirectAbstractDeclarator),
);

box_all!(DirectAbstractDeclarator,
    Parens(AbstractDeclarator),
    UnsizedArray(),
    SizedArray(ConstantExpression),
    UnsizedArrayMore(DirectAbstractDeclarator),
    SizedArrayMore(DirectAbstractDeclarator, ConstantExpression),
    EmptyParens(),
    ParameterList(ParameterTypeList),
    EmptyParensMore(),
    ParameterListMore(ParameterTypeList),
);

box_all!(Initializer,
    AssignmentExpression(AssignmentExpression),
    InitializerList([Initializer])
);

box_all!(Statement,
    LabeledStatement(LabeledStatement),
    CompoundStatement([Declaration], [Statement]),
    ExpressionStatement(ExpressionStatement),
    SelectionStatement(SelectionStatement),
    IterationStatement(IterationStatement),
    JumpStatement(JumpStatement)
);

box_all!(LabeledStatement,
    GotoLabel(Identifier, Statement),
    Case(Identifier, Statement),
    DefaultCase(Statement),
);

box_all!(ExpressionStatement,
    Empty(),
    Expression(Expression)
);

box_all!(SelectionStatement,
    If(Expression, Statement),
    IfElse(Expression, Statement, Statement),
    Switch(Expression, Statement)
);

box_all!(IterationStatement,
    While(Expression, Statement),
    Do(Statement, Expression),
    For(ExpressionStatement, ExpressionStatement, Statement),
    ForDo(ExpressionStatement, ExpressionStatement, Expression, Statement)
);

box_all!(JumpStatement,
    Goto(Identifier),
    Continue(),
    Break(),
    Return(),
    ReturnExpression(Expression)
);

box_all!(TranslationUnit,
    ExternalDeclaration([ExternalDeclaration])
);

box_all!(ExternalDeclaration,
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration)
);

box_all!(FunctionDefinition,
    DeclarationSpecifiers(DeclarationSpecifiers, Declarator, [Declaration], [Declaration], [Statement]),
    Declaration(Declarator, [Declaration], [Declaration], [Statement])
);

*/
*/

#[derive(Debug)]
pub enum TranslationUnit<'a> {
    ExternalDeclaration(&'a [i64]) 
}
