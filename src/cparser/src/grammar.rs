use std::rc::Rc;

use ctoken::token::Token;

grammar! {
    S -> TranslationUnit;

    PrimaryExpression
       -> #Token::Identifier
        | #Token::Number
        | #Token::StringLiteral
        | #Token::CharLiteral
        | #Token::Sizeof
        | Statement. #Token::OpenParen &CompoundStatement #Token::CloseParen
        | Expression. #Token::OpenParen &Expression #Token::CloseParen
        | StructValue. #Token::OpenParen &TypeName #Token::CloseParen #Token::OpenBrace &InitializerList #Token::CloseBrace
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum PrimaryExpression {
            Identifier(Rc<str>),
            Builtin(Box<Builtin>),
            Number(Rc<str>),
            StringLiteral(Rc<str>),
            CharLiteral(char),
            Sizeof(Box<[Token]>),
            Statement(Box<CompoundStatement>),
            Expression(Box<Expression>),
            StructValue(Box<TypeName>, Vec<Initializer>),
        }
        ;

    PostfixExpression
       -> PrimaryExpression
        | Index. PostfixExpression #Token::OpenBracket &Expression #Token::CloseBracket
        | Call.PostfixExpression #Token::OpenParen ArgumentExpressionList #Token::CloseParen
        | Member. PostfixExpression MemberAccess #Token::Identifier
        | Increment. PostfixExpression IncrementOrDecrement
        ;

    MemberAccess
       -> #Token::Dot
        | #Token::Arrow
        ;

    IncrementOrDecrement
       -> #Token::Increment
        | #Token::Decrement
        ;

    ArgumentExpressionList
       -> Epsilon
        | &NonemptyArgumentExpressionList
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum ArgumentExpressionList { List(Vec<AssignmentExpression>) }
        ;

    NonemptyArgumentExpressionList
       -> &AssignmentExpression
        | &NonemptyArgumentExpressionList #Token::Comma &AssignmentExpression
        ;

    UnaryExpression
       -> &PostfixExpression
        | IncrementOrDecrement UnaryExpression
        | UnaryOperator &CastExpression
        | SizeofExpr. #Token::Sizeof UnaryExpression
        | SizeofTy. #Token::Sizeof #Token::OpenParen TypeName #Token::CloseParen
        | AddressOfLabel. #Token::And #Token::Identifier
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum UnaryExpression {
            PostfixExpression(Box<PostfixExpression>),
            IncrementOrDecrement(IncrementOrDecrement, Box<UnaryExpression>),
            UnaryOperator(UnaryOperator, Box<CastExpression>),
            SizeofExpr(Box<UnaryExpression>),
            SizeofTy(Box<TypeName>),
            AddressOfLabel(Rc<str>),
        }
        ;

    CastExpression
      -> UnaryExpression
       | #Token::OpenParen &TypeName #Token::CloseParen &GeneralExpression
       ;

    GeneralExpression
       -> &CastExpression
        | 12: Times. GeneralExpression #Token::Times GeneralExpression
        | 12: Divide. GeneralExpression #Token::Divide GeneralExpression
        | 12: Mod. GeneralExpression #Token::Mod GeneralExpression
        | 11: Plus. GeneralExpression #Token::Plus GeneralExpression
        | 11: Minus. GeneralExpression #Token::Minus GeneralExpression
        | 10: BitShiftLeft. GeneralExpression #Token::BitShiftLeft GeneralExpression
        | 10: BitShiftRight. GeneralExpression #Token::BitShiftRight GeneralExpression
        | 9: LessThan. GeneralExpression #Token::LessThan GeneralExpression
        | 9: MoreThan. GeneralExpression #Token::MoreThan GeneralExpression
        | 9: LessEqThan. GeneralExpression #Token::LessEqThan GeneralExpression
        | 9: MoreEqThan. GeneralExpression #Token::MoreEqThan GeneralExpression
        | 8: Equals. GeneralExpression #Token::Equals GeneralExpression
        | 8: NotEquals. GeneralExpression #Token::NotEquals GeneralExpression
        | 7: BitAnd. GeneralExpression #Token::BitAnd GeneralExpression
        | 6: BitXor. GeneralExpression #Token::BitXor GeneralExpression
        | 5: BitOr. GeneralExpression #Token::BitOr GeneralExpression
        | 4: And. GeneralExpression #Token::And GeneralExpression
        | 3: Or. GeneralExpression #Token::Or GeneralExpression
        ;

    TernaryExpression
       -> GeneralExpression
        | Ternary. GeneralExpression #Token::Ternary &Expression #Token::Colon TernaryExpression
        ;

    AssignmentExpression
       -> TernaryExpression
        | Assignment. &UnaryExpression AssignmentOperator AssignmentExpression
        ;

    UnaryOperator
       -> #Token::BitAnd
        | #Token::Times
        | #Token::Plus
        | #Token::Minus
        | #Token::BitNot
        | #Token::Not
        ;

    AssignmentOperator
       -> #Token::Assign
        | #Token::TimesAssign
        | #Token::DivAssign
        | #Token::ModAssign
        | #Token::PlusAssign
        | #Token::MinusAssign
        | #Token::BitShiftLeftAssign
        | #Token::BitShiftRightAssign
        | #Token::BitAndAssign
        | #Token::BitXorAssign
        | #Token::BitOrAssign
        ;

    Expression
       -> &AssignmentExpression
        | Comma. Expression #Token::Comma &AssignmentExpression
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Expression { Expression(Vec<AssignmentExpression>) }
        ;

    TypeDeclaration
       -> &DeclarationSpecifiers #Token::Semicolon
        | List. #Token::Typedef &DeclarationSpecifiers &InitDeclaratorList #Token::Semicolon
        ;

    InitDeclaratorList
       -> &InitDeclarator
        | Comma. InitDeclaratorList #Token::Comma &InitDeclarator
        ;

    InitDeclarator
       -> &Declarator
        | Asm. &Declarator &AsmStatement
        | Assign. &Declarator #Token::Assign &AssignmentOrInitializerList
        ;

    Sign -> #Token::Signed | #Token::Unsigned;
    MaybeInt -> Epsilon | #Token::Int;

    StructOrUnionSpecifier
       -> NewType. StructOrUnion #Token::Identifier #Token::OpenBrace &StructDeclarationList #Token::CloseBrace
        | Anonymous. StructOrUnion #Token::OpenBrace &StructDeclarationList #Token::CloseBrace
        | NameOnly. StructOrUnion #Token::Identifier
        ;

    StructOrUnion
       -> #Token::Struct
        | #Token::Union
        ;

    StructDeclarationList
       -> &StructDeclaration
        | StructDeclarationList &StructDeclaration
        | Epsilon
        ;

    StructDeclaration
       -> &SpecifierQualifierList &StructDeclaratorList #Token::Semicolon
        | Anonymous. &SpecifierQualifierList #Token::Semicolon
        ;

    SpecifierQualifierList
       -> Left. Qualifiers &TypeSpecifier
        | Both. Qualifiers &TypeSpecifier Qualifiers
        | Right. &TypeSpecifier Qualifiers
        | Neither. &TypeSpecifier
        ;

    StructDeclaratorList
       -> StructDeclarator
        | StructDeclaratorList #Token::Comma StructDeclarator
        ;

    StructDeclarator
       -> &Declarator
        | BitField. #Token::Colon &GeneralExpression
        | NamedBitField. &Declarator #Token::Colon &GeneralExpression
        ;

    EnumSpecifier
       -> #Token::Enum MaybeIdentifier #Token::OpenBrace &EnumeratorList #Token::CloseBrace
        | ExistingType. #Token::Enum #Token::Identifier
        ;

    MaybeIdentifier
       -> Epsilon
        | #Token::Identifier
        ;

    EnumeratorListContent
       -> Enumerator
        | EnumeratorListContent #Token::Comma Enumerator
        ;

    EnumeratorList
       -> EnumeratorListContent TrailingComma
        ;

    Enumerator
       -> #Token::Identifier
        | Assign. #Token::Identifier #Token::Assign &TernaryExpression
        ;

    MaybeGeneralExpression
       -> Epsilon
        | GeneralExpression
        ;

    FunctionParams
       -> Epsilon
        | &ParameterTypeList
        | &IdentifierList
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum FunctionParam {
            Identifier(Rc<str>),
            Parameter(ParameterDeclaration),
            Varargs
        }
        ;

    TypeQualifierList
       -> TypeQualifiers
        | TypeQualifierList TypeQualifiers
        ;

    Qualifiers
       -> TypeQualifiers
        | TypeQualifierList. TypeQualifiers Qualifiers
        ;


    ParameterTypeList
       -> ParameterList
        | Varargs. ParameterList #Token::Comma #Token::Varargs
        ;

    ParameterList
       -> ParameterDeclaration
        | ParameterList #Token::Comma ParameterDeclaration
        ;

    ParameterDeclaration
       -> Declarator. &DeclarationSpecifiers &Declarator
        | AbstractDeclarator. &DeclarationSpecifiers &AbstractDeclarator
        | &DeclarationSpecifiers
        ;

    IdentifierList
       -> #Token::Identifier
        | IdentifierList #Token::Comma #Token::Identifier
        ;

    TypeName
       -> &SpecifierQualifierList
        | AbstractDeclarator. &SpecifierQualifierList &AbstractDeclarator
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum TypeName {
            TypeName(Box<DeclarationSpecifiers>, Box<AbstractDeclarator>),
        }
        ;

    MaybeParameterTypeList
       -> Epsilon
        | &ParameterTypeList
        ;

    AssignmentOrInitializerList
       -> AssignmentExpression
        | #Token::OpenBrace InitializerList #Token::CloseBrace
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum AssignmentOrInitializerList {
            AssignmentExpression(AssignmentExpression),
            Initializers(Vec<Initializer>)
        }
        ;

    Initializer
       -> &AssignmentOrInitializerList
        | #Token::Dot #Token::Identifier #Token::Assign &AssignmentOrInitializerList
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Initializer {
            Initializer(Option<Rc<str>>, Box<AssignmentOrInitializerList>),
        }
        ;

    InitializerListContent
       -> &Initializer
        | &InitializerListContent #Token::Comma &Initializer
        ;

    InitializerList
       -> &InitializerListContent TrailingComma
        | Epsilon
        ;

    TrailingComma
       -> #Token::Comma
        | Epsilon
        ;

    Statement
       -> &LabeledStatement
        | &CompoundStatement
        | &ExpressionStatement
        | &SelectionStatement
        | &IterationStatement
        | &JumpStatement
        | &TypeDeclaration
        | &AsmStatement
        ;

    LabeledStatement
       -> #Token::Identifier #Token::Colon Statement
        | Case. #Token::Case &GeneralExpression #Token::Colon Statement
        | RangeCase. #Token::Case &GeneralExpression #Token::Varargs &GeneralExpression #Token::Colon Statement
        | #Token::Default #Token::Colon Statement
        ;

    CompoundStatement
       -> #Token::OpenBrace &StatementList #Token::CloseBrace
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum CompoundStatement { Statements(Vec<StatementOrDeclaration>) }
        ;

    DeclarationList
       -> Declaration
        | DeclarationList Declaration
        ;

    StatementList
       -> StatementOrDeclaration
        | More. StatementList StatementOrDeclaration
        | Epsilon
        ;

    StatementOrDeclaration
       -> Statement
        | Declaration
        ;

    ExpressionStatement
       -> #Token::Semicolon
        | &Expression #Token::Semicolon
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum ExpressionStatement { Expression(Option<Box<Expression>>) }
        ;

    SelectionStatement
       -> #Token::If #Token::OpenParen &Expression #Token::CloseParen Statement &MaybeElse
        | #Token::Switch #Token::OpenParen &Expression #Token::CloseParen Statement
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum SelectionStatement {
            If(Box<Expression>, Statement, Option<Box<Statement>>),
            Switch(Box<Expression>, Statement),
        }
        ;

    MaybeElse
       -> Epsilon
        | #Token::Else Statement
        ;

    IterationStatement
       -> #Token::While #Token::OpenParen &Expression #Token::CloseParen &Statement
        | #Token::Do &Statement #Token::While #Token::OpenParen &Expression #Token::CloseParen #Token::Semicolon
        | #Token::For #Token::OpenParen ForExpr #Token::CloseParen &Statement
        ;

    ForExpr
       -> EmptyLast. &ExpressionStatement &ExpressionStatement
        | &ExpressionStatement &ExpressionStatement &Expression
        ;

    JumpStatement
       -> #Token::Goto #Token::Identifier #Token::Semicolon
        | #Token::Continue #Token::Semicolon
        | #Token::Break #Token::Semicolon
        | ReturnVoid. #Token::Return #Token::Semicolon
        | #Token::Return Expression #Token::Semicolon
        ;

    AsmStatement
       -> #Token::Asm #Token::OpenParen /* anything here */ #Token::CloseParen
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum AsmStatement { Asm { tokens: Vec<Token>, volatile: bool, goto: bool, inline: bool } }
        ;

    TranslationUnit
       -> ExternalDeclaration
        | TranslationUnit ExternalDeclaration
        | Epsilon
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum TranslationUnit { Units(Vec<ExternalDeclaration>) }
        ;

    ExternalDeclaration
       -> &FunctionDefinition
        | &Declaration
        | &TypeDeclaration
        | #Token::Semicolon
        ;

    FunctionDefinition
       -> SpecifiersDeclarations. &DeclarationSpecifiers &Declarator &DeclarationList &CompoundStatement
        | Specifiers. &DeclarationSpecifiers &Declarator &CompoundStatement
        | Declarations. &Declarator &DeclarationList &CompoundStatement
        | Declarator. &Declarator &CompoundStatement
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum FunctionDefinition { FunctionDefinition(Option<Box<DeclarationSpecifiers>>, Vec<Declarator>, Box<CompoundStatement>) }
        ;

    DeclarationSpecifiers
       -> Left. &Specifiers &TypeSpecifier
        | Right. &TypeSpecifier &Specifiers
        | Both. &Specifiers &TypeSpecifier &Specifiers
        | Neither. &TypeSpecifier
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum DeclarationSpecifiers { DeclarationSpecifiers(Option<Specifiers>, Option<TypeSpecifier>) }
        ;

    StorageClassSpecifiers
       -> #Token::Extern
        | #Token::Static
        | #Token::Inline
        | #Token::Auto
        | #Token::Register
        | #Token::Typedef
        @ #[derive(Clone, Debug, Default, PartialEq)]
        pub struct StorageClassSpecifiers {
            pub typedef: bool,
            pub extern_: bool,
            pub static_: bool,
            pub inline: bool,
            pub auto: bool,
            pub register: bool,
        }
        ;

    TypeQualifiers
       -> #Token::Const
        | #Token::Volatile
        @ #[derive(Clone, Debug, Default, PartialEq)]
        pub struct TypeQualifiers {
            pub const_: bool,
            pub volatile: bool,
        }
        ;

    TypeSpecifier
       -> #Token::Void
        | Char. #Token::Char
        | SignedChar. Sign #Token::Char
        | Short. #Token::Short MaybeInt
        | SignedShort. Sign #Token::Short MaybeInt
        | Int. #Token::Int
        | SignedInt. Sign #Token::Int
        | Long. #Token::Long MaybeInt
        | SignedLong. Sign #Token::Long MaybeInt
        | LongLong. #Token::Long #Token::Long MaybeInt
        | SignedLongLong. Sign #Token::Long #Token::Long MaybeInt
        | Signed. Sign
        | #Token::Float
        | #Token::Double
        | &StructOrUnionSpecifier
        | &EnumSpecifier
        | #Token::Identifier
        | TypeOf
        @ pub type TypeSpecifier = CType
        ;

    TypeOf
       -> Expression. #Token::Identifier #Token::OpenParen &Expression #Token::CloseParen
        | TypeName. #Token::Identifier #Token::OpenParen &TypeName #Token::CloseParen
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum TypeOf {
            Expression(Box<Expression>),
            TypeName(Box<TypeName>),
        }
        ;

    Specifiers
       -> TypeQualifiers
        | StorageClassSpecifiers
        | StorageClassSpecifierList. StorageClassSpecifiers Specifiers
        | TypeQualifierList. TypeQualifiers Specifiers
        @ pub type Specifiers = (StorageClassSpecifiers, TypeQualifiers)
        ;

    Pointer
       -> #Token::Times
        | TypeList. #Token::Times TypeQualifierList
        | Pointer. #Token::Times Pointer
        | TypeListPointer. #Token::Times TypeQualifierList Pointer
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Pointer {
            Ptr(TypeQualifiers, Option<Box<Pointer>>),
        }
        ;

    Declaration
       -> &DeclarationSpecifiers #Token::Semicolon
        | List. &DeclarationSpecifiers &InitDeclaratorList #Token::Semicolon
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Declaration {
            Declaration(Box<DeclarationSpecifiers>, Vec<InitDeclarator>),
        }
        ;

    AbstractDeclarator
       -> &Pointer &DirectAbstractDeclarator
        | &DirectAbstractDeclarator
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum AbstractDeclarator {
            AbstractDeclarator(Option<Box<Pointer>>, Box<DirectAbstractDeclarator>),
        }
        ;

    Declarator
       -> &Pointer &DirectDeclarator
        | &DirectDeclarator
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Declarator {
            Declarator(Option<Box<Pointer>>, Box<DirectDeclarator>),
        }
        ;

    DirectAbstractDeclarator
       -> Epsilon
        | #Token::OpenParen &AbstractDeclarator #Token::CloseParen
        | Array. DirectAbstractDeclarator #Token::OpenBracket MaybeGeneralExpression #Token::CloseBracket
        | Function. DirectAbstractDeclarator #Token::OpenParen &MaybeParameterTypeList #Token::CloseParen
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum DirectAbstractDeclarator {
            Epsilon(),
            Parens(Box<AbstractDeclarator>),
            Array(Box<DirectAbstractDeclarator>, Option<Box<GeneralExpression>>),
            Function(Box<DirectAbstractDeclarator>, FunctionParams),
        }
        ;

    DirectDeclarator
       -> #Token::Identifier
        | #Token::OpenParen &Declarator #Token::CloseParen
        | Array. DirectDeclarator #Token::OpenBracket MaybeGeneralExpression #Token::CloseBracket
        | FunctionParams. DirectDeclarator #Token::OpenParen FunctionParams #Token::CloseParen
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum DirectDeclarator {
            Identifier(Rc<str>),
            Parens(Box<Declarator>),
            Array(Box<DirectDeclarator>, Option<Box<GeneralExpression>>),
            Function(Box<DirectDeclarator>, FunctionParams),
        }
        ;

}

pub type FunctionParams = Vec<FunctionParam>;

pub type TypeSign = Option<bool>;

#[derive(Clone, Debug, PartialEq)]
pub enum CType {
    Void,
    Primitive(TypeSign, PrimitiveType),
    Compound(CompoundType),
    Custom(Rc<str>),
    TypeOf(TypeOf),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PrimitiveType {
    Char,
    Short,
    Int,
    Long,
    LongLong,
    Float,
    Double,
    LongDouble,
}

#[derive(Clone, Debug, PartialEq)]
pub enum EitherDeclarator {
    Anonymous(AbstractDeclarator),
    Declarator(Declarator),
}

pub type StructField = (
    Box<DeclarationSpecifiers>,
    Option<Vec<(EitherDeclarator, Option<Box<GeneralExpression>>)>>,
);
pub type EnumField = (Rc<str>, Option<TernaryExpression>);

#[derive(Clone, Debug, PartialEq)]
pub enum CompoundType {
    Struct(Rc<str>, Option<Vec<StructField>>),
    AnonymousStruct(Vec<StructField>),
    Union(Rc<str>, Option<Vec<StructField>>),
    AnonymousUnion(Vec<StructField>),
    Enum(Rc<str>, Option<Vec<EnumField>>),
    AnonymousEnum(Vec<EnumField>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Builtin {
    Offsetof(Box<TypeName>, Box<BuiltinDesignator>),
    TypesCompatible(Box<TypeName>, Box<TypeName>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BuiltinDesignator {
    Identifier(Rc<str>),
    Field(Box<BuiltinDesignator>, Rc<str>),
    Index(Box<BuiltinDesignator>, Box<Expression>),
}

impl StorageClassSpecifiers {
    pub fn any(&self) -> bool {
        self.typedef | self.extern_ | self.static_ | self.inline | self.auto | self.register
    }
}

impl TypeQualifiers {
    pub fn any(&self) -> bool {
        self.const_ | self.volatile
    }
}
