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

fn ident_to_name(token: Token) -> String {
    match token {
        Token::Identifier(_, s) => s,
        t => panic!("Internal compiler error: Unexpected token: {:?}, expected identifier", t)
    }
}

fn get_direct_decl_identifier(decl: DirectDeclarator) -> Vec<String> {
    match decl {
        DirectDeclarator::Identifier(DirectDeclarator_Identifier(t)) => vec![ident_to_name(t)],
        DirectDeclarator::OpenParen(DirectDeclarator_OpenParen(_, decl, _)) => get_decl_identifier(*decl),
        DirectDeclarator::SizedArray(DirectDeclarator_SizedArray(decl, ..)) => get_direct_decl_identifier(*decl),
        DirectDeclarator::Array(DirectDeclarator_Array(decl, ..)) => get_direct_decl_identifier(*decl),
        DirectDeclarator::FunctionParams(DirectDeclarator_FunctionParams(decl, ..)) => get_direct_decl_identifier(*decl),
    }
}

fn get_decl_identifier(decl: Declarator) -> Vec<String> {
    let direct_decl = match decl {
        Declarator::Pointer(Declarator_Pointer(_, direct_decl)) => vec![direct_decl],
        Declarator::DirectDeclarator(Declarator_DirectDeclarator(direct_decl)) => vec![direct_decl],
    };
    direct_decl.into_iter().flat_map(|s| get_direct_decl_identifier(s).into_iter()).collect()
}

fn add_type(state: &mut State, name: String) {
    println!("Adding new type {}", name);
    state.insert(name);
}

fn add_decl(state: &mut State, init_decl: InitDeclarator) {
    let decl = match init_decl {
        InitDeclarator::Declarator(InitDeclarator_Declarator(decl)) => decl,
        InitDeclarator::Asm(..) => panic!("Cannot typedef asm"),
        InitDeclarator::Assign(..) => panic!("Assignment to a typedef")
    };

    get_decl_identifier(decl).into_iter()
        .for_each(|name| add_type(state, name));
}

fn add_typedef(state: &mut State, declaration: TypeDeclaration) {
    let (_spec, list) = match declaration {
        TypeDeclaration::Typedef(TypeDeclaration_Typedef(..)) => {
            println!("Warning: useless typedef");
            return;
        },
        TypeDeclaration::List(TypeDeclaration_List(_, spec, list, ..)) => (spec, list)
    };
    
    let decls = get_decls(list);
    decls.into_iter().for_each(|decl| add_decl(state, decl));
}

fn identifier_or_type_to_str(ty: IdentifierOrType) -> String {
    match ty {
        IdentifierOrType::Identifier(IdentifierOrType_Identifier(token)) => ident_to_name(token),
        IdentifierOrType::TypeNameStr(IdentifierOrType_TypeNameStr(token)) => ident_to_name(token),
    }
}

fn add_struct_type(state: &mut State, declaration: Box<StructOrUnionSpecifier>) {
    match *declaration {
        StructOrUnionSpecifier::NewType(StructOrUnionSpecifier_NewType(_, ty, ..)) =>
            add_type(state, identifier_or_type_to_str(ty)),
        StructOrUnionSpecifier::Anonymous(StructOrUnionSpecifier_Anonymous(..)) => {}
        StructOrUnionSpecifier::NameOnly(StructOrUnionSpecifier_NameOnly(_, ty, ..)) =>
            add_type(state, identifier_or_type_to_str(ty)),
    };
}

fn add_enum_type(state: &mut State, declaration: Box<EnumSpecifier>) {
    match *declaration {
        EnumSpecifier::List(EnumSpecifier_List(_, _, ..)) => {}
        EnumSpecifier::Empty(EnumSpecifier_Empty(_, token, ..)) =>
            add_type(state, ident_to_name(token)),
        EnumSpecifier::NameOnly(EnumSpecifier_NameOnly(_, token, ..)) =>
            add_type(state, ident_to_name(token)),
        EnumSpecifier::ExistingType(..) => {}
    };
}

fn is_type(state: &State, token: &Token) -> bool {
    match token {
        Token::Identifier(_, i) => match state.get(i) {
            Some(_) => {
                println!("Identifier {} is a type", i);
                true
            }
            None => {
                println!("Identifier {} is not a type", i);
                false
            }
        }
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
        | #Token::CharLiteral
        | #Token::Sizeof
        | #Token::Asm
        | Statement. #Token::OpenParen &CompoundStatement #Token::CloseParen
        | Expression. #Token::OpenParen &Expression #Token::CloseParen
        ;

    PostfixExpression
        -> PrimaryExpression
        | Index. PostfixExpression #Token::OpenBracket Expression #Token::CloseBracket
        | Call. PostfixExpression #Token::OpenParen #Token::CloseParen
        | CallWithArguments.PostfixExpression #Token::OpenParen ArgumentExpressionList #Token::CloseParen
        | Member. PostfixExpression #Token::Dot #Token::Identifier
        | Dereference. PostfixExpression #Token::Arrow #Token::Identifier
        | Increment. PostfixExpression #Token::Increment
        | Decrement. PostfixExpression #Token::Decrement
        | StructValue. #Token::OpenParen TypeName #Token::CloseParen #Token::OpenBrace InitializerList #Token::CloseBrace
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
        ;

    CastExpression
      -> UnaryExpression
       | #Token::OpenParen TypeName #Token::CloseParen GeneralExpression
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
        | Ternary. GeneralExpression #Token::Terniary &Expression #Token::Colon TernaryExpression
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
        | Asm. Declarator #Token::Asm
        | Assign. Declarator #Token::Assign Initializer
        ;

    StorageClassSpecifier
       -> #Token::Extern
        | #Token::Static
        | #Token::Inline
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

    IdentifierOrType
        -> #Token::Identifier
         | #TypeNameStr;

    StructOrUnionSpecifier
       -> NewType. StructOrUnion IdentifierOrType #Token::OpenBrace StructDeclarationList #Token::CloseBrace
        | Anonymous. StructOrUnion #Token::OpenBrace StructDeclarationList #Token::CloseBrace
        | NameOnly. StructOrUnion IdentifierOrType        ;

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
        | Anonymous. SpecifierQualifierList #Token::Semicolon
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
        | BitField. #Token::Colon GeneralExpression
        | NamedBitField. Declarator #Token::Colon GeneralExpression
        ;

    EnumSpecifier
       -> List. #Token::Enum #Token::OpenBrace EnumeratorList #Token::CloseBrace
        | Empty. #Token::Enum #Token::Identifier #Token::OpenBrace EnumeratorList #Token::CloseBrace
        | ExistingType. #Token::Enum #TypeNameStr
        | NameOnly. #Token::Enum #Token::Identifier
        ;

    EnumeratorList
       -> Enumerator
        | EnumeratorList #Token::Comma Enumerator
        | TrailingComma. EnumeratorList #Token::Comma
        ;

    Enumerator
       -> #Token::Identifier
        | Assign. #Token::Identifier #Token::Assign GeneralExpression
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
        | SizedArray. DirectDeclarator #Token::OpenBracket GeneralExpression #Token::CloseBracket
        | Array. DirectDeclarator #Token::OpenBracket #Token::CloseBracket
        | FunctionParams. DirectDeclarator #Token::OpenParen FunctionParams #Token::CloseParen
        ;

    FunctionParams
       -> &ParameterTypeList
        | &IdentifierList
        | Epsilon;

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
        | SizedArray. #Token::OpenBracket GeneralExpression #Token::CloseBracket
        | AbstractArray. DirectAbstractDeclarator #Token::OpenBracket #Token::CloseBracket
        | AbstractSizedArray. DirectAbstractDeclarator #Token::OpenBracket GeneralExpression #Token::CloseBracket
        | Function. #Token::OpenParen #Token::CloseParen
        | FunctionParams. #Token::OpenParen &ParameterTypeList #Token::CloseParen
        | AbstractFunction. DirectAbstractDeclarator #Token::OpenParen #Token::CloseParen
        | AbstractFunctionParams. DirectAbstractDeclarator #Token::OpenParen &ParameterTypeList #Token::CloseParen
        ;

    Initializer
       -> AssignmentExpression
        | #Token::OpenBrace InitializerList #Token::CloseBrace
        | TrailingComma. #Token::OpenBrace InitializerList #Token::Comma #Token::CloseBrace
        | #Token::Dot #Token::Identifier #Token::Assign AssignmentExpression
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
        | Case. #Token::Case GeneralExpression #Token::Colon Statement
        | RangeCase. #Token::Case GeneralExpression #Token::Varargs GeneralExpression #Token::Colon Statement
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
        | !add_typedef TypeDeclaration
        ;

    FunctionDefinition
       -> SpecifiersDeclarations. DeclarationSpecifiers Declarator DeclarationList CompoundStatement
        | Specifiers. DeclarationSpecifiers Declarator CompoundStatement
        | Declarations. Declarator DeclarationList CompoundStatement
        | Declarator CompoundStatement
        ;
}
