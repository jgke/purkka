use std::collections::HashSet;

use ctoken::token::Token;

type State = HashSet<String>;

fn get_decls(decl: InitDeclaratorList) -> Vec<InitDeclarator> {
    match decl {
        InitDeclaratorList::InitDeclarator(InitDeclaratorList_InitDeclarator(decl)) => vec![decl],
        InitDeclaratorList::Comma(InitDeclaratorList_Comma(more, _, decl)) => {
            let mut decls = get_decls(*more);
            decls.push(decl);
            decls
        }
    }
}

fn get_direct_decl_identifier(decl: DirectDeclarator) -> String {
    match decl {
        DirectDeclarator::Identifier(DirectDeclarator_Identifier(Token::Identifier(s))) => s,
        DirectDeclarator::Identifier(DirectDeclarator_Identifier(_)) => unreachable!(),
        DirectDeclarator::OpenParen(DirectDeclarator_OpenParen(_, decl, _)) => get_decl_identifier(*decl),
        DirectDeclarator::SizedArray(DirectDeclarator_SizedArray(decl, ..)) => get_direct_decl_identifier(*decl),
        DirectDeclarator::Array(DirectDeclarator_Array(decl, ..)) => get_direct_decl_identifier(*decl),
        DirectDeclarator::FunctionParams(DirectDeclarator_FunctionParams(decl, ..)) => get_direct_decl_identifier(*decl),
        DirectDeclarator::FunctionIdents(DirectDeclarator_FunctionIdents(decl, ..)) => get_direct_decl_identifier(*decl),
        DirectDeclarator::Function(DirectDeclarator_Function(decl, ..)) => get_direct_decl_identifier(*decl),
    }
}

fn get_decl_identifier(decl: Declarator) -> String {
    let direct_decl = match decl {
        Declarator::Pointer(Declarator_Pointer(_, direct_decl)) => direct_decl,
        Declarator::DirectDeclarator(Declarator_DirectDeclarator(direct_decl)) => direct_decl,
    };
    get_direct_decl_identifier(direct_decl)
}

fn add_decl(state: &mut State, init_decl: InitDeclarator) {
    let decl = match init_decl {
        InitDeclarator::Declarator(InitDeclarator_Declarator(decl)) => decl,
        InitDeclarator::Assign(InitDeclarator_Assign(..)) => panic!("Assignment to a typedef")
    };
    let name = get_decl_identifier(decl);
    state.insert(name);
}

fn add_type(state: &mut State, declaration: TypeDeclaration) {
    let (spec, list) = match declaration {
        TypeDeclaration::Typedef(TypeDeclaration_Typedef(_, spec, ..)) => {
            println!("Warning: useless typedef");
            return;
        },
        TypeDeclaration::List(TypeDeclaration_List(_, spec, list, ..)) => (spec, list)
    };
    
    let decls = get_decls(list);
    decls.into_iter().for_each(|decl| add_decl(state, decl));
}

fn is_type(state: &State, token: &Token) -> bool {
    match token {
        Token::Identifier(i) => state.get(i).is_some(),
        _ => panic!()
    }
}

lalr! {
    !TypeNameStr -> is_type #Token::Identifier;

    S -> TranslationUnit;

    PrimaryExpression
       -> #Token::Identifier
        | #Token::Number
        | #Token::StringLiteral
        | #Token::OpenParen &Expression #Token::CloseParen
        ;

    PostfixExpression
        -> PrimaryExpression
        | Index. PostfixExpression #Token::OpenBracket Expression #Token::CloseBracket
        | Call. PostfixExpression #Token::OpenParen #Token::CloseParen
        | CallWithArguments.PostfixExpression #Token::OpenParen ArgumentExpressionList #Token::CloseParen
        | Member. PostfixExpression #Token::Dot #Token::Identifier
        | Dereference. PostfixExpression #Token::Times #Token::Identifier
        | Increment. PostfixExpression #Token::Increment
        | Decrement. PostfixExpression #Token::Decrement
        ;

    ArgumentExpressionList
       -> AssignmentExpression
        | ArgumentExpressionList #Token::Comma AssignmentExpression
        ;

    UnaryExpression
       -> PostfixExpression
        | #Token::Increment UnaryExpression
        | #Token::Decrement UnaryExpression
        | UnaryOperator &CastExpression
        | #Token::Sizeof UnaryExpression
        | SizeofParen. #Token::Sizeof #Token::OpenParen TypeName #Token::CloseParen
        ;

    UnaryOperator
       -> #Token::BitAnd
        | #Token::Times
        | #Token::Plus
        | #Token::Minus
        | #Token::BitNot
        | #Token::Not
        ;

    CastExpression
       -> UnaryExpression
        | #Token::OpenParen TypeName #Token::CloseParen CastExpression
        ;

    MultiplicativeExpression
       -> CastExpression
        | Times. MultiplicativeExpression #Token::Times CastExpression
        | Divide. MultiplicativeExpression #Token::Divide CastExpression
        | Mod. MultiplicativeExpression #Token::Mod CastExpression
        ;

    AdditiveExpression
       -> MultiplicativeExpression
        | Plus. AdditiveExpression #Token::Plus MultiplicativeExpression
        | Minus. AdditiveExpression #Token::Minus MultiplicativeExpression
        ;

    ShiftExpression
       -> AdditiveExpression
        | BitShiftLeft. ShiftExpression #Token::BitShiftLeft AdditiveExpression
        | BitShiftRight. ShiftExpression #Token::BitShiftRight AdditiveExpression
        ;

    RelationalExpression
       -> ShiftExpression
        | LessThan. RelationalExpression #Token::LessThan ShiftExpression
        | MoreThan. RelationalExpression #Token::MoreThan ShiftExpression
        ;

    EqualityExpression
       -> RelationalExpression
        | Equals. EqualityExpression #Token::Equals RelationalExpression
        | NotEquals. EqualityExpression #Token::NotEquals RelationalExpression
        ;

    AndExpression
       -> EqualityExpression
        | BitAnd. AndExpression #Token::BitAnd EqualityExpression
        ;

    ExclusiveOrExpression
       -> AndExpression
        | BitXor. ExclusiveOrExpression #Token::BitXor AndExpression
        ;

    InclusiveOrExpression
       -> ExclusiveOrExpression
        | BitOr. InclusiveOrExpression #Token::BitOr ExclusiveOrExpression
        ;

    LogicalAndExpression
       -> InclusiveOrExpression
        | And. LogicalAndExpression #Token::And InclusiveOrExpression
        ;

    LogicalOrExpression
       -> LogicalAndExpression
        | Or. LogicalOrExpression #Token::Or LogicalAndExpression
        ;

    ConditionalExpression
       -> &LogicalOrExpression
        | Ternary. &LogicalOrExpression #Token::Terniary &Expression #Token::Colon &ConditionalExpression
        ;

    AssignmentExpression
       -> ConditionalExpression
        | Assignment. &UnaryExpression AssignmentOperator AssignmentExpression
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
        ;

    ConstantExpression
       -> &ConditionalExpression
        ;

    Declaration
       -> DeclarationSpecifiers #Token::Semicolon
        | List. DeclarationSpecifiers InitDeclaratorList #Token::Semicolon
        ;

    TypeDeclaration
       -> #Token::Typedef DeclarationSpecifiers #Token::Semicolon
        | List. #Token::Typedef DeclarationSpecifiers InitDeclaratorList #Token::Semicolon
        ;

    DeclarationSpecifiers
       -> StorageClassSpecifier
        | StorageList. StorageClassSpecifier DeclarationSpecifiers
        | TypeSpecifier
        | SpecifierList. TypeSpecifier DeclarationSpecifiers
        | TypeQualifier
        | QualifierList. TypeQualifier DeclarationSpecifiers
        ;

    InitDeclaratorList
       -> InitDeclarator
        | Comma. InitDeclaratorList #Token::Comma InitDeclarator
        ;

    InitDeclarator
       -> Declarator
        | Assign. Declarator #Token::Assign Initializer
        ;

    StorageClassSpecifier
       -> #Token::Extern
        | #Token::Static
        | #Token::Auto
        | #Token::Register
        ;

    TypeSpecifier
       -> #Token::Void
        | #Token::Char
        | #Token::Short
        | #Token::Int
        | #Token::Long
        | #Token::Float
        | #Token::Double
        | #Token::Signed
        | #Token::Unsigned
        | &StructOrUnionSpecifier
        | &EnumSpecifier
        | #TypeNameStr
        ;

    StructOrUnionSpecifier
       -> List. StructOrUnion #Token::Identifier #Token::OpenBrace StructDeclarationList #Token::CloseBrace
        | Empty. StructOrUnion #Token::OpenBrace StructDeclarationList #Token::CloseBrace
        | NameOnly. StructOrUnion #Token::Identifier
        ;

    StructOrUnion
       -> #Token::Struct
        | #Token::Union
        ;

    StructDeclarationList
       -> StructDeclaration
        | StructDeclarationList StructDeclaration
        ;

    StructDeclaration
       -> SpecifierQualifierList StructDeclaratorList #Token::Semicolon
        ;

    SpecifierQualifierList
       -> TypeSpecifierList. TypeSpecifier SpecifierQualifierList
        | TypeSpecifier
        | TypeQualifierList. TypeQualifier SpecifierQualifierList
        | TypeQualifier
        ;

    StructDeclaratorList
       -> StructDeclarator
        | StructDeclaratorList #Token::Comma StructDeclarator
        ;

    StructDeclarator
       -> Declarator
        | BitField. #Token::Colon ConstantExpression
        | NamedBitField. Declarator #Token::Colon ConstantExpression
        ;

    EnumSpecifier
       -> List. #Token::Enum #Token::OpenBrace EnumeratorList #Token::CloseBrace
        | Empty. #Token::Enum #Token::Identifier #Token::OpenBrace EnumeratorList #Token::CloseBrace
        | NameOnly. #Token::Enum #Token::Identifier
        ;

    EnumeratorList
       -> Enumerator
        | EnumeratorList #Token::Comma Enumerator
        ;

    Enumerator
       -> #Token::Identifier
        | Assign. #Token::Identifier #Token::Assign ConstantExpression
        ;

    TypeQualifier
       -> #Token::Const
        | #Token::Volatile
        ;

    Declarator
       -> Pointer DirectDeclarator
        | DirectDeclarator
        ;

    DirectDeclarator
       -> #Token::Identifier
        | #Token::OpenParen &Declarator #Token::CloseParen
        | SizedArray. DirectDeclarator #Token::OpenBracket ConstantExpression #Token::CloseBracket
        | Array. DirectDeclarator #Token::OpenBracket #Token::CloseBracket
        | FunctionParams. DirectDeclarator #Token::OpenParen &ParameterTypeList #Token::CloseParen
        | FunctionIdents. DirectDeclarator #Token::OpenParen IdentifierList #Token::CloseParen
        | Function. DirectDeclarator #Token::OpenParen #Token::CloseParen
        ;

    Pointer
       -> #Token::Times
        | TypeList. #Token::Times TypeQualifierList
        | Pointer. #Token::Times Pointer
        | TypeListPointer. #Token::Times TypeQualifierList Pointer
        ;

    TypeQualifierList
       -> TypeQualifier
        | TypeQualifierList TypeQualifier
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
        | AbstractDeclarator. DeclarationSpecifiers AbstractDeclarator
        | DeclarationSpecifiers
        ;

    IdentifierList
       -> #Token::Identifier
        | IdentifierList #Token::Comma #Token::Identifier
        ;

    TypeName
       -> SpecifierQualifierList
        | AbstractDeclarator. SpecifierQualifierList AbstractDeclarator
        ;

    AbstractDeclarator
       -> Pointer
        | DirectAbstractDeclarator
        | Both. Pointer DirectAbstractDeclarator
        ;

    DirectAbstractDeclarator
       -> #Token::OpenParen &AbstractDeclarator #Token::CloseParen
        | Array. #Token::OpenBracket #Token::CloseBracket
        | SizedArray. #Token::OpenBracket ConstantExpression #Token::CloseBracket
        | AbstractArray. DirectAbstractDeclarator #Token::OpenBracket #Token::CloseBracket
        | AbstractSizedArray. DirectAbstractDeclarator #Token::OpenBracket ConstantExpression #Token::CloseBracket
        | Function. #Token::OpenParen #Token::CloseParen
        | FunctionParams. #Token::OpenParen &ParameterTypeList #Token::CloseParen
        | AbstractFunction. DirectAbstractDeclarator #Token::OpenParen #Token::CloseParen
        | AbstractFunctionParams. DirectAbstractDeclarator #Token::OpenParen &ParameterTypeList #Token::CloseParen
        ;

    Initializer
       -> AssignmentExpression
        | #Token::OpenBrace InitializerList #Token::CloseBrace
        | TrailingComma. #Token::OpenBrace InitializerList #Token::Comma #Token::CloseBrace
        ;

    InitializerList
       -> &Initializer
        | InitializerList #Token::Comma &Initializer
        ;

    Statement
       -> &LabeledStatement
        | &CompoundStatement
        | &ExpressionStatement
        | &SelectionStatement
        | &IterationStatement
        | &JumpStatement
        ;

    LabeledStatement
       -> #Token::Identifier #Token::Colon Statement
        | #Token::Case ConstantExpression #Token::Colon Statement
        | #Token::Default #Token::Colon Statement
        ;

    CompoundStatement
       -> Empty. #Token::OpenBrace #Token::CloseBrace
        | Statements. #Token::OpenBrace &StatementList #Token::CloseBrace
        | Declarations. #Token::OpenBrace &DeclarationList #Token::CloseBrace
        | Both. #Token::OpenBrace &DeclarationList &StatementList #Token::CloseBrace
        ;

    DeclarationList
       -> Declaration
        | DeclarationList Declaration
        ;

    StatementList
       -> Statement
        | StatementList Statement
        ;

    ExpressionStatement
       -> #Token::Semicolon
        | Expression #Token::Semicolon
        ;

    SelectionStatement
       -> #Token::If #Token::OpenParen Expression #Token::CloseParen Statement
        | IfElse. #Token::If #Token::OpenParen Expression #Token::CloseParen Statement #Token::Else Statement
        | #Token::Switch #Token::OpenParen Expression #Token::CloseParen Statement
        ;

    IterationStatement
       -> #Token::While #Token::OpenParen Expression #Token::CloseParen Statement
        | #Token::Do Statement #Token::While #Token::OpenParen Expression #Token::CloseParen #Token::Semicolon
        | ForEmptyLast. #Token::For #Token::OpenParen ExpressionStatement ExpressionStatement #Token::CloseParen Statement
        | #Token::For #Token::OpenParen ExpressionStatement ExpressionStatement Expression #Token::CloseParen Statement
        ;

    JumpStatement
       -> #Token::Goto #Token::Identifier #Token::Semicolon
        | #Token::Continue #Token::Semicolon
        | #Token::Break #Token::Semicolon
        | ReturnVoid. #Token::Return #Token::Semicolon
        | #Token::Return Expression #Token::Semicolon
        ;

    TranslationUnit
       -> ExternalDeclaration
        | TranslationUnit ExternalDeclaration
        | Epsilon
        ;

    ExternalDeclaration
       -> FunctionDefinition
        | Declaration
        | !add_type TypeDeclaration
        ;

    FunctionDefinition
       -> SpecifiersDeclarations. DeclarationSpecifiers Declarator DeclarationList CompoundStatement
        | Specifiers. DeclarationSpecifiers Declarator CompoundStatement
        | Declarations. Declarator DeclarationList CompoundStatement
        | Declarator CompoundStatement
        ;
}
