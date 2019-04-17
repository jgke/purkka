use std::collections::HashSet;

use ctoken::token::Token;

#[derive(Debug)]
pub struct ScopedState {
    pub types: HashSet<String>,
    pub labels: HashSet<String>,
}

#[derive(Debug)]
pub struct State {
    pub scope: Vec<ScopedState>,
}

fn get_decls(decl: InitDeclaratorList) -> Vec<InitDeclarator> {
    match decl {
        InitDeclaratorList::InitDeclarator(decl) => vec![decl],
        InitDeclaratorList::Comma(more, _, decl) => {
            let mut decls = get_decls(*more);
            decls.push(decl);
            decls
        }
    }
}

fn ident_to_name(token: Token) -> String {
    match token {
        Token::Identifier(_, s) => s,
        t => panic!(
            "Internal compiler error: Unexpected token: {:?}, expected identifier",
            t
        ),
    }
}

fn get_decl_identifier(decl: Declarator) -> Vec<String> {
    match decl {
        Declarator::Pointer(_, t, _) => vec![identifier_or_type_to_str(t)],
        Declarator::Identifier(t, _) => vec![ident_to_name(t)],
        Declarator::FunctionPointer(_, decl, ..) => get_decl_identifier(*decl),
        Declarator::FunctionPointerReturningPointer(_, _, decl, ..) => get_decl_identifier(*decl),
    }
}

fn add_type(state: &mut State, name: String) {
    println!("Adding new type {}", name);
    state.scope.last_mut().unwrap().types.insert(name);
}

fn add_decl(state: &mut State, init_decl: InitDeclarator) {
    let decl = match init_decl {
        InitDeclarator::Declarator(decl) => decl,
        InitDeclarator::Asm(..) => panic!("Cannot typedef asm"),
        InitDeclarator::Assign(..) => panic!("Assignment to a typedef"),
    };

    get_decl_identifier(decl)
        .into_iter()
        .for_each(|name| add_type(state, name));
}

fn add_typedef(state: &mut State, declaration: Box<TypeDeclaration>) {
    let (_spec, list) = match *declaration {
        TypeDeclaration::Typedef(..) => {
            println!("Warning: useless typedef");
            return;
        }
        TypeDeclaration::List(_, spec, list, ..) => (spec, list),
    };

    let decls = get_decls(list);
    decls.into_iter().for_each(|decl| add_decl(state, decl));
}

fn identifier_or_type_to_str(ty: IdentifierOrType) -> String {
    match ty {
        IdentifierOrType::Identifier(token) => ident_to_name(token),
        IdentifierOrType::TypeNameStr(token) => ident_to_name(token),
    }
}

fn add_struct_type(state: &mut State, declaration: Box<StructOrUnionSpecifier>) {
    match *declaration {
        StructOrUnionSpecifier::NewType(_, ty, ..) => {
            add_type(state, identifier_or_type_to_str(ty))
        }
        StructOrUnionSpecifier::Anonymous(..) => {}
        StructOrUnionSpecifier::NameOnly(_, ty, ..) => {
            add_type(state, identifier_or_type_to_str(ty))
        }
    };
}

fn add_enum_type(state: &mut State, declaration: Box<EnumSpecifier>) {
    match *declaration {
        EnumSpecifier::List(_, _, ..) => {}
        EnumSpecifier::Empty(_, token, ..) => add_type(state, ident_to_name(token)),
        EnumSpecifier::NameOnly(_, token, ..) => add_type(state, ident_to_name(token)),
        EnumSpecifier::ExistingType(..) => {}
    };
}

fn is_type(state: &State, token: &Token) -> bool {
    match token {
        Token::Identifier(_, i) => state.scope.iter().any(|s| s.types.contains(i)),
        _ => panic!(),
    }
}

fn is_label(state: &State, token: &Token) -> bool {
    match token {
        Token::Identifier(_, i) => state.scope.iter().any(|s| s.labels.contains(i)),
        _ => panic!(),
    }
}

fn maybe_add_label(
    state: &mut State,
    decl_spec: DeclarationSpecifiers,
    init_list: InitDeclaratorList,
    _semicolon: Token,
) {
    match decl_spec {
        DeclarationSpecifiers::TypeSpecifier(TypeSpecifier::TypeNameStr(t)) => {
            let ty = ident_to_name(t);
            if ty == "__label__" {
                let labels = &mut state.scope.last_mut().unwrap().labels;
                get_decls(init_list).into_iter().for_each(|init_decl| {
                    let decl = match init_decl {
                        InitDeclarator::Declarator(decl) => decl,
                        InitDeclarator::Asm(..) => panic!("Invalid __label__"),
                        InitDeclarator::Assign(..) => panic!("Invalid __label__"),
                    };

                    get_decl_identifier(decl).into_iter().for_each(|name| {
                        labels.insert(name);
                    });
                });
            }
        }
        _ => {}
    }
}

fn push_scope(state: &mut State, _open_brace: Token) {
    state.scope.push(ScopedState {
        types: HashSet::new(),
        labels: HashSet::new(),
    });
}

fn pop_scope(state: &mut State, _open: PushScope, _statements: Box<StatementList>, _close: Token) {
    state.scope.pop();
}

lalr! {
    !TypeNameStr -> is_type #Token::Identifier;
    !Label -> is_label #Token::Identifier;

    S -> TranslationUnit;

    PrimaryExpression
       -> #Token::Identifier
        | #Token::Number
        | #Token::StringLiteral
        | #Token::CharLiteral
        | #Token::Sizeof
        | #Token::Asm
        | #Token::And #Label // Weird GCC extension around label values
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
        ;

    Declaration
       -> DeclarationSpecifiers #Token::Semicolon
        | !maybe_add_label List. DeclarationSpecifiers InitDeclaratorList #Token::Semicolon
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
        | Assign. Declarator #Token::Assign AssignmentOrInitializerList
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

    IdentifierOrLabel
        -> #Token::Identifier
         | #Label;

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
        | Epsilon
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

    EnumeratorListContent
       -> Enumerator
        | EnumeratorListContent #Token::Comma Enumerator
        ;

    EnumeratorList
       -> EnumeratorListContent TrailingComma
        ;

    Enumerator
       -> #Token::Identifier
        | Assign. #Token::Identifier #Token::Assign TernaryExpression
        ;

    TypeQualifier
       -> #Token::Const
        | #Token::Volatile
        ;

    Declarator
       -> Pointer IdentifierOrType DirectDeclarator
        | #Token::Identifier DirectDeclarator
        | FunctionPointer. #Token::OpenParen &Declarator #Token::CloseParen DirectDeclarator
        | FunctionPointerReturningPointer. Pointer #Token::OpenParen &Declarator #Token::CloseParen DirectDeclarator
        ;

    DirectDeclarator
       -> Epsilon
        | SizedArray. DirectDeclarator #Token::OpenBracket GeneralExpression #Token::CloseBracket
        | Array. DirectDeclarator #Token::OpenBracket #Token::CloseBracket
        | Function. DirectDeclarator #Token::OpenParen #Token::CloseParen
        | FunctionParams. DirectDeclarator #Token::OpenParen FunctionParams #Token::CloseParen
        ;

    FunctionParams
       -> &ParameterTypeList
        | &IdentifierList
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
        | SizedArray. #Token::OpenBracket GeneralExpression #Token::CloseBracket
        | AbstractArray. DirectAbstractDeclarator #Token::OpenBracket #Token::CloseBracket
        | AbstractSizedArray. DirectAbstractDeclarator #Token::OpenBracket GeneralExpression #Token::CloseBracket
        | Function. #Token::OpenParen #Token::CloseParen
        | FunctionParams. #Token::OpenParen &ParameterTypeList #Token::CloseParen
        | AbstractFunction. DirectAbstractDeclarator #Token::OpenParen #Token::CloseParen
        | AbstractFunctionParams. DirectAbstractDeclarator #Token::OpenParen &ParameterTypeList #Token::CloseParen
        ;

    AssignmentOrInitializerList
       -> AssignmentExpression
        | #Token::OpenBrace InitializerList #Token::CloseBrace
        ;

    Initializer
       -> AssignmentOrInitializerList
        | #Token::Dot #Token::Identifier #Token::Assign AssignmentOrInitializerList
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
        | !add_typedef &TypeDeclaration
        ;

    LabeledStatement
       -> IdentifierOrLabel #Token::Colon Statement
        | Case. #Token::Case GeneralExpression #Token::Colon Statement
        | RangeCase. #Token::Case GeneralExpression #Token::Varargs GeneralExpression #Token::Colon Statement
        | #Token::Default #Token::Colon Statement
        ;

    PushScope
       -> !push_scope #Token::OpenBrace
        ;

    CompoundStatement
       -> !pop_scope PushScope &StatementList #Token::CloseBrace
        ;

    DeclarationList
       -> Declaration
        | DeclarationList Declaration
        ;

    StatementList
       -> Statement. Statement
        | Declaration. Declaration
        | StatementMore. StatementList Statement
        | DeclarationMore. StatementList Declaration
        | Epsilon
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
       -> &FunctionDefinition
        | &Declaration
        | !add_typedef &TypeDeclaration
        | #Token::Semicolon
        ;

    FunctionDefinition
       -> SpecifiersDeclarations. DeclarationSpecifiers Declarator DeclarationList CompoundStatement
        | Specifiers. DeclarationSpecifiers Declarator CompoundStatement
        | Declarations. Declarator DeclarationList CompoundStatement
        | Declarator CompoundStatement
        ;
}
