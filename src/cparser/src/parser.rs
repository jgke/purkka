use std::collections::HashSet;
use std::fmt::Debug;
use std::rc::Rc;

use debug::debug::{if_debug, DebugVal};
use shared::traits::{multipeek, MultiPeek};

use ctoken::token::Token;

use crate::grammar::*;
use crate::*;

macro_rules! maybe_read_token {
    ($iter:expr, $tok:path) => {
        if let Some($tok(..)) = $iter.peek() {
            $iter.next()
        } else {
            None
        }
    };
}

macro_rules! unexpected_token {
    ($token:expr, $iter:expr) => {
        match $token {
            None => panic!("Unexpected end of file"),
            Some(t) => {
                let as_s = format!("{:?}", t);
                let num = t.get_num();
                panic!(
                    "Unexpected token: {}\n{}",
                    as_s,
                    $iter.fragment.source_to_str(&$iter.sources[num])
                );
            }
        }
    };
}

macro_rules! unexpected_token_expected_one {
    ($token:expr, $iter:expr, $expected:path) => {
        match $token {
            None => panic!("Unexpected end of file, expected {}", stringify!($expected)),
            Some(t) => {
                let as_s = format!("{:?}", t);
                let expected = stringify!($expected);
                let num = t.get_num();
                panic!(
                    "Unexpected token: {}, expected {}\n{}",
                    as_s,
                    expected,
                    $iter.fragment.source_to_str(&$iter.sources[num])
                );
            }
        }
    };
}

macro_rules! read_token {
    ($iter:expr, $tok:path) => {
        if let Some($tok(..)) = $iter.peek() {
            $iter.next().unwrap().clone()
        } else {
            unexpected_token_expected_one!(&$iter.next(), $iter, $tok)
        }
    };
}

macro_rules! operation_left {
    ($left:ident $this:ident $target:ident) => {{
        $left = Box::new(GeneralExpression::$target(
            $left,
            $this.next().cloned().unwrap(),
            Box::new(GeneralExpression::CastExpression(Box::new(
                $this.parse_cast_expression(),
            ))),
        ));
    }};
}

macro_rules! operation_right {
    ($left:ident $this:ident $prio:literal) => {{
        $left = $this.parse_general_expression_($prio, $left);
    }};
}

#[derive(Debug, Default)]
pub struct ScopedState {
    pub types: HashSet<Rc<str>>,
    pub labels: HashSet<Rc<str>>,
}

#[derive(Debug)]
pub struct ParseContext<'a, I>
where I: Iterator<Item=&'a Token>
{
    pub scope: Vec<ScopedState>,
    pub iter: MultiPeek<I>,
    pub fragment: &'a FragmentIterator,
    pub sources: &'a [Source],
}

fn default_types() -> ScopedState {
    let mut types = HashSet::new();

    types.insert(From::from("va_list"));
    types.insert(From::from("__builtin_va_list"));
    types.insert(From::from("size_t"));
    types.insert(From::from("_Bool"));
    types.insert(From::from("_Complex"));

    ScopedState {
        types,
        labels: HashSet::new(),
    }
}

pub fn parse<'a, I>(
    iter: I,
    sources: &'a [Source],
    fragment_iter: &'a FragmentIterator,
) -> Result<S, Option<Token>>
where I: IntoIterator<Item=&'a Token>
{
    println!("Starting parsing");
    let context = ParseContext {
        scope: vec![default_types()],
        iter: multipeek(iter),
        fragment: fragment_iter,
        sources,
    };
    let context_mut = std::sync::Mutex::new(context);

    std::panic::catch_unwind(|| {
        S::TranslationUnit(context_mut.lock().unwrap().parse_translation_unit())
    })
    .map_err(|_| match context_mut.lock() {
        Ok(mut context) => context.iter.peek().cloned().cloned(),
        Err(context) => context.into_inner().iter.peek().cloned().cloned(),
    })
}

impl<'a, I> ParseContext<'a, I>
where I: Iterator<Item=&'a Token> {
    fn next(&mut self) -> Option<&'a Token> {
        if_debug(DebugVal::CParserToken, || {
            dbg!(self.peek());
            if let Some(Token::Identifier(_, i)) = self.peek() {
                dbg!(self.is_type(i));
            }
        });
        match self.peek() {
            Some(Token::OpenBrace(..)) => self.push_scope(),
            Some(Token::CloseBrace(..)) => self.pop_scope(),
            _ => {}
        }
        self.iter.next()
    }
    fn peek(&mut self) -> Option<&&'a Token> {
        self.iter.peek()
    }
    fn peek_n(&mut self, n: usize) -> Vec<&&'a Token> {
        self.iter.peek_n(n)
    }
    fn push_scope(&mut self) {
        self.scope.push(ScopedState::default());
    }
    fn pop_scope(&mut self) {
        self.scope.pop().unwrap();
    }

    fn parse_translation_unit(&mut self) -> TranslationUnit {
        let mut units = Vec::new();
        while maybe_read_token!(self, Token::Semicolon).is_some() {}
        while self.peek().is_some() {
            units.push(self.parse_external_declaration());
            while maybe_read_token!(self, Token::Semicolon).is_some() {}
        }
        TranslationUnit::Units(units)
    }

    fn parse_external_declaration(&mut self) -> ExternalDeclaration {
        let spec = self.parse_declaration_specifiers();
        let has_typedef = if let DeclarationSpecifiers::DeclarationSpecifiers(Some(spec), _) = &spec {
            spec.0.typedef
        } else {
            false
        };
        if let Some(Token::Semicolon(..)) = self.peek() {
            return ExternalDeclaration::Declaration(Box::new(Declaration::Declaration(
                Box::new(spec),
                vec![],
            )));
        }
        let declarator = self.parse_declarator();

        let ext_decl = match self.peek() {
            Some(Token::OpenBrace(..)) => {
                let block = Box::new(self.parse_compound_statement());
                return ExternalDeclaration::FunctionDefinition(Box::new(
                    FunctionDefinition::FunctionDefinition(
                        Some(Box::new(spec)),
                        vec![declarator],
                        block,
                    ),
                ));
            }
            Some(Token::Semicolon(..)) => {
                ExternalDeclaration::Declaration(Box::new(Declaration::Declaration(
                    Box::new(spec),
                    vec![InitDeclarator::Declarator(Box::new(declarator))],
                )))
            }
            _ => ExternalDeclaration::Declaration(Box::new(Declaration::Declaration(
                Box::new(spec),
                self.parse_init_declarator_list(declarator),
            ))),
        };

        read_token!(self, Token::Semicolon);

        if has_typedef {
            for t in self.ext_decl_identifiers(&ext_decl) {
                self.add_type(t);
            }
        }
        ext_decl
    }

    fn add_type(&mut self, t: Rc<str>) {
        self.scope.last_mut().unwrap().types.insert(t);
    }

    fn parse_type_name(&mut self) -> TypeName {
        let specs = self.parse_declaration_specifiers();
        let abs = self.parse_abstract_declarator();
        TypeName::TypeName(Box::new(specs), Box::new(abs))
    }

    fn parse_abstract_declarator(&mut self) -> AbstractDeclarator {
        let ptr = match_first!(
            self.peek() => _t,
            default None,

            Pointer => self.parse_ptr(),
        );
        let abs = match_first!(
            self.peek() => _t,
            default Box::new(DirectAbstractDeclarator::Epsilon()),

            DirectAbstractDeclarator => self.parse_direct_abstract_declarator(),
        );
        AbstractDeclarator::AbstractDeclarator(ptr, abs)
    }

    fn parse_direct_abstract_declarator(&mut self) -> Box<DirectAbstractDeclarator> {
        let mut decl = if let Some(Token::OpenParen(..)) = self.peek() {
            read_token!(self, Token::OpenParen);
            let d = self.parse_abstract_declarator();
            read_token!(self, Token::CloseParen);
            Box::new(DirectAbstractDeclarator::Parens(Box::new(d)))
        } else {
            Box::new(DirectAbstractDeclarator::Epsilon())
        };
        loop {
            match self.peek() {
                Some(Token::OpenBracket(..)) => {
                    read_token!(self, Token::OpenBracket);
                    let expr = match self.peek() {
                        Some(Token::CloseBracket(..)) => None,
                        _ => Some(self.parse_general_expression()),
                    };
                    read_token!(self, Token::CloseBracket);
                    decl = Box::new(DirectAbstractDeclarator::Array(decl, expr));
                }
                Some(Token::OpenParen(..)) => {
                    read_token!(self, Token::OpenParen);
                    let params = self.parse_params();
                    read_token!(self, Token::CloseParen);
                    decl = Box::new(DirectAbstractDeclarator::Function(decl, params));
                }
                _ => break,
            }
        }
        decl
    }

    fn parse_declaration_specifiers(&mut self) -> DeclarationSpecifiers {
        let mut spec = (StorageClassSpecifiers::default(), TypeQualifiers::default());
        self.parse_specifiers(&mut spec);
        let ty = self.parse_type_specifier();
        self.parse_specifiers(&mut spec);

        let opt_spec = if spec.0.any() || spec.1.any() {
            Some(spec)
        } else {
            None
        };

        DeclarationSpecifiers::DeclarationSpecifiers(opt_spec, ty)
    }

    fn parse_specifiers(&mut self, spec: &mut Specifiers) {
        loop {
            match self.peek() {
                Some(Token::Typedef(..)) => spec.0.typedef = true,
                Some(Token::Extern(..)) => spec.0.extern_ = true,
                Some(Token::Static(..)) => spec.0.static_ = true,
                Some(Token::Inline(..)) => spec.0.inline = true,
                Some(Token::Auto(..)) => spec.0.auto = true,
                Some(Token::Register(..)) => spec.0.register = true,
                Some(Token::Const(..)) => spec.1.const_ = true,
                Some(Token::Volatile(..)) => spec.1.volatile = true,
                _ => break,
            }
            self.next();
        }
    }

    fn parse_type_specifier(&mut self) -> Option<TypeSpecifier> {
        let mut longs = 0;
        let mut sign = None;
        let mut data = None;
        let mut ty = None;

        loop {
            match self.peek().cloned() {
                Some(Token::Void(..)) => {
                    read_token!(self, Token::Void);
                    ty.replace_if_empty(CType::Void, "Conflicting type specifiers");
                }
                Some(Token::Short(..)) => {
                    read_token!(self, Token::Short);
                    data.replace_if_empty(PrimitiveType::Short, "Conflicting type specifiers");
                }
                Some(Token::Int(..)) => {
                    read_token!(self, Token::Int);
                    data.replace_if_empty(PrimitiveType::Int, "Conflicting type specifiers");
                }
                Some(Token::Char(..)) => {
                    read_token!(self, Token::Char);
                    data.replace_if_empty(PrimitiveType::Char, "Conflicting type specifiers");
                }
                Some(Token::Signed(..)) => {
                    read_token!(self, Token::Signed);
                    sign.replace_if_empty(true, "Conflicting type specifiers")
                }
                Some(Token::Unsigned(..)) => {
                    read_token!(self, Token::Unsigned);
                    sign.replace_if_empty(false, "Conflicting type specifiers")
                }
                Some(Token::Float(..)) => {
                    read_token!(self, Token::Float);
                    data.replace_if_empty(PrimitiveType::Float, "Conflicting type specifiers");
                }
                Some(Token::Double(..)) => {
                    read_token!(self, Token::Double);
                    data.replace_if_empty(PrimitiveType::Double, "Conflicting type specifiers");
                }
                Some(Token::Long(..)) => {
                    read_token!(self, Token::Long);
                    longs += 1;
                }
                Some(Token::Enum(..)) | Some(Token::Struct(..)) | Some(Token::Union(..)) => {
                    ty.replace_if_empty(
                        CType::Compound(self.parse_compound_type()),
                        "Conflicting type specifiers",
                    );
                }
                Some(Token::Identifier(_, ident)) if self.is_typeof(ident.as_ref()) => {
                    read_token!(self, Token::Identifier);
                    read_token!(self, Token::OpenParen);
                    let next = self.peek().cloned();
                    dbg!(next);
                    let r = if self.starts_type(next.unwrap()) {
                        TypeOf::TypeName(Box::new(self.parse_type_name()))
                    } else {
                        TypeOf::Expression(Box::new(self.parse_expression()))
                    };
                    read_token!(self, Token::CloseParen);
                    ty.replace_if_empty(
                        CType::TypeOf(r),
                        "Conflicting type specifiers")
                }
                Some(Token::Identifier(_, ident)) if self.is_type(ident) => {
                    read_token!(self, Token::Identifier);
                    ty.replace_if_empty(
                        CType::Custom(ident.clone()),
                        "Conflicting type specifiers",
                    );
                }
                _ => break,
            }
        }

        if longs == 0 && sign.is_none() && data.is_none() && ty.is_none() {
            None
        } else {
            match (longs, sign, data, ty) {
                (0, None, None, Some(ty)) => Some(ty),
                (_, _, _, Some(_)) => panic!("Conflicting type specifiers"),

                (0, sign, Some(primitive), None) => Some(CType::Primitive(sign, primitive)),
                (0, sign, None, None) => Some(CType::Primitive(sign, PrimitiveType::Int)),

                (1, sign, Some(PrimitiveType::Int), None) | (1, sign, None, None) => {
                    Some(CType::Primitive(sign, PrimitiveType::Long))
                }
                (2, sign, Some(PrimitiveType::Int), None) | (2, sign, None, None) => {
                    Some(CType::Primitive(sign, PrimitiveType::LongLong))
                }

                (_, Some(_), Some(PrimitiveType::Float), _)
                | (_, Some(_), Some(PrimitiveType::Double), _) => {
                    panic!("Signed floats are not supported")
                }
                (1, None, Some(PrimitiveType::Double), None) => {
                    Some(CType::Primitive(None, PrimitiveType::LongDouble))
                }

                /* t > 2 */
                (t, _, _, _) if t > 2 => panic!("Type is too long! ({})", t),
                _ => unreachable!(),
            }
        }
    }

    fn parse_compound_type(&mut self) -> CompoundType {
        if let Some(Token::Enum(..)) = self.peek() {
            read_token!(self, Token::Enum);
            let ident = maybe_read_token!(self, Token::Identifier);
            match self.peek() {
                Some(Token::OpenBrace(..)) => {
                    read_token!(self, Token::OpenBrace);
                    let mut fields = vec![self.parse_enum_field()];
                    while let Some(Token::Comma(..)) = self.peek() {
                        read_token!(self, Token::Comma);
                        if let Some(Token::CloseBrace(..)) = self.peek() {
                            break;
                        }
                        fields.push(self.parse_enum_field());
                    }
                    read_token!(self, Token::CloseBrace);
                    match ident {
                        Some(t) => CompoundType::Enum(t.get_ident_str().clone(), Some(fields)),
                        None => CompoundType::AnonymousEnum(fields),
                    }
                }
                _ => match ident {
                    Some(t) => CompoundType::Enum(t.get_ident_str().clone(), None),
                    None => unexpected_token!(self.peek(), self),
                },
            }
        } else {
            let is_union = maybe_read_token!(self, Token::Union).is_some();
            if !is_union {
                read_token!(self, Token::Struct);
            }
            let ident = maybe_read_token!(self, Token::Identifier);
            match self.peek() {
                Some(Token::OpenBrace(..)) => {
                    read_token!(self, Token::OpenBrace);
                    let mut fields = vec![];
                    loop {
                        match self.peek() {
                            Some(Token::Semicolon(..)) => {
                                read_token!(self, Token::Semicolon);
                            }
                            Some(Token::CloseBrace(..)) => break,
                            _ => {
                                fields.push(self.parse_struct_field());
                                read_token!(self, Token::Semicolon);
                            }
                        }
                    }
                    read_token!(self, Token::CloseBrace);

                    match ident {
                        Some(t) => if is_union {
                            CompoundType::Union(t.get_ident_str().clone(), Some(fields))
                        } else {
                            CompoundType::Struct(t.get_ident_str().clone(), Some(fields))
                        }
                        None => if is_union {
                            CompoundType::AnonymousUnion(fields)
                        } else {
                            CompoundType::AnonymousStruct(fields)
                        }
                    }
                }
                _ => match ident {
                    Some(t) => if is_union {
                        CompoundType::Union(t.get_ident_str().clone(), None)
                    } else {
                        CompoundType::Struct(t.get_ident_str().clone(), None)
                    }
                    None => unexpected_token!(self.peek(), self),
                },
            }
        }
    }

    fn parse_enum_field(&mut self) -> (Rc<str>, Option<TernaryExpression>) {
        let ident = read_token!(self, Token::Identifier).get_ident_str().clone();
        if let Some(Token::Assign(..)) = self.peek() {
            read_token!(self, Token::Assign);
            let expr = self.parse_ternary_expression();
            (ident, Some(expr))
        } else {
            (ident, None)
        }
    }

    fn parse_struct_field(&mut self) -> (Box<DeclarationSpecifiers>, Option<Vec<(EitherDeclarator, Option<Box<GeneralExpression>>)>>) {
        let specifiers = Box::new(self.parse_declaration_specifiers());
        match self.peek() {
            Some(Token::Semicolon(..)) => (specifiers, None),
            _ => {
                let either = self.parse_declarator_or_abstract_declarator();
                let expr = match self.peek() {
                    Some(Token::Colon(..)) => {
                        read_token!(self, Token::Colon);
                        Some(self.parse_general_expression())
                    }
                    _ => None
                };
                let mut decls = vec![(either, expr)];
                loop {
                    if let Some(Token::Comma(..)) = self.peek() {
                        read_token!(self, Token::Comma);
                        let either = self.parse_declarator_or_abstract_declarator();
                        let expr = match self.peek() {
                            Some(Token::Colon(..)) => {
                                read_token!(self, Token::Colon);
                                Some(self.parse_general_expression())
                            }
                            _ => None
                        };
                        decls.push((either, expr));
                    } else {
                        break
                    }
                }
                (specifiers, Some(decls))
            }
        }
    }

    fn parse_declarator(&mut self) -> Declarator {
        let ptr = self.parse_ptr();
        Declarator::Declarator(ptr, self.parse_direct_declarator())
    }

    /* Detect whether we should parse a declarator or an abstract declarator, using arbitrary
     * lookahead. I think there's some method of doing this without n-lookahead, but this is a lot
     * easier :) */
    fn is_declarator_upcoming(&mut self) -> bool {
        self.peek_pointer();
        self.is_direct_declarator_upcoming()
    }

    /* Consume a pointer from the peek buf */
    fn peek_pointer(&mut self) {
        match self.iter.peek_buf() {
            Some(Token::Times(..)) => {
                self.iter.peek_and_advance();
                loop {
                    match self.iter.peek_buf() {
                        Some(Token::Volatile(..)) | Some(Token::Const(..)) => self.iter.peek_and_advance(),
                        _ => break,
                    };
                }
                self.peek_pointer();
            }
            _ => {}
        }
    }

    fn is_direct_declarator_upcoming(&mut self) -> bool {
        match self.iter.peek_and_advance() {
            Some(Token::Identifier(..)) => true,
            Some(Token::OpenParen(..)) => self.is_declarator_upcoming(),
            _ => false,
        }
    }

    fn parse_declarator_or_abstract_declarator(&mut self) -> EitherDeclarator {
        self.peek(); /* Clear out peek buf, just in case */
        if self.is_declarator_upcoming() {
            EitherDeclarator::Declarator(self.parse_declarator())
        } else {
            EitherDeclarator::Anonymous(self.parse_abstract_declarator())
        }
    }


    fn parse_ptr(&mut self) -> Option<Box<Pointer>> {
        match self.peek() {
            Some(Token::Times(..)) => {
                read_token!(self, Token::Times);
                let mut spec = TypeQualifiers::default();
                loop {
                    match self.peek() {
                        Some(Token::Const(..)) => spec.const_ = true,
                        Some(Token::Volatile(..)) => spec.volatile = true,
                        _ => break,
                    }
                    self.next();
                }
                Some(Box::new(Pointer::Ptr(spec, self.parse_ptr())))
            }
            _ => None,
        }
    }

    fn parse_direct_declarator(&mut self) -> Box<DirectDeclarator> {
        let mut decl = if let Some(Token::Identifier(..)) = self.peek() {
            Box::new(DirectDeclarator::Identifier(read_token!(self, Token::Identifier).get_ident_str().clone()))
        } else {
            read_token!(self, Token::OpenParen);
            let d = self.parse_declarator();
            read_token!(self, Token::CloseParen);
            Box::new(DirectDeclarator::Parens(Box::new(d)))
        };
        loop {
            match self.peek() {
                Some(Token::OpenBracket(..)) => {
                    read_token!(self, Token::OpenBracket);
                    let expr = match self.peek() {
                        Some(Token::CloseBracket(..)) => None,
                        _ => Some(self.parse_general_expression()),
                    };
                    read_token!(self, Token::CloseBracket);
                    decl = Box::new(DirectDeclarator::Array(decl, expr));
                }
                Some(Token::OpenParen(..)) => {
                    read_token!(self, Token::OpenParen);
                    let params = self.parse_params();
                    read_token!(self, Token::CloseParen);
                    decl = Box::new(DirectDeclarator::Function(decl, params));
                }
                _ => break,
            }
        }
        decl
    }

    fn parse_params(&mut self) -> Vec<FunctionParam> {
        let mut params = Vec::new();

        loop {
            match self.peek() {
                Some(Token::CloseParen(..)) => break,
                _ => {
                    let spec = Box::new(self.parse_declaration_specifiers());
                    match self.peek() {
                        Some(Token::Varargs(..)) => {
                            read_token!(self, Token::Varargs);
                            params.push(FunctionParam::Varargs);
                            break;
                        }
                        Some(Token::Comma(..)) | Some(Token::CloseParen(..))
                            => params.push(FunctionParam::Parameter(ParameterDeclaration::DeclarationSpecifiers(spec))),
                        _ => {
                            let decl = match self.parse_declarator_or_abstract_declarator() {
                                EitherDeclarator::Anonymous(declarator)
                                    => ParameterDeclaration::AbstractDeclarator(spec, Box::new(declarator)),
                                EitherDeclarator::Declarator(declarator)
                                    => ParameterDeclaration::Declarator(spec, Box::new(declarator)),
                            };
                            params.push(FunctionParam::Parameter(decl));
                        }
                    };
                    match self.peek() {
                        Some(Token::Comma(..)) => read_token!(self, Token::Comma),
                        Some(Token::CloseParen(..)) => break,
                        t => unexpected_token!(t, self),
                    };
                }
            }
        }

        params
    }

    fn parse_init_declarator_list(&mut self, declarator: Declarator) -> Vec<InitDeclarator> {
        let mut decls = vec![self.parse_init_declarator(declarator)];
        while let Some(Token::Comma(..)) = self.peek() {
            read_token!(self, Token::Comma);
            let decl = self.parse_declarator();
            decls.push(self.parse_init_declarator(decl));
        }
        decls
    }

    fn parse_init_declarator(&mut self, decl: Declarator) -> InitDeclarator {
        match self.peek() {
            Some(Token::Asm(..)) => {
                let asm = read_token!(self, Token::Asm);
                InitDeclarator::Asm(Box::new(decl), asm)
            }
            Some(Token::Assign(..)) => {
                let t = read_token!(self, Token::Assign);
                let list = self.parse_assignment_or_initializer_list();
                InitDeclarator::Assign(Box::new(decl), t, Box::new(list))
            }
            _ => InitDeclarator::Declarator(Box::new(decl)),
        }
    }

    fn parse_assignment_or_initializer_list(&mut self) -> AssignmentOrInitializerList {
        match self.peek() {
            Some(Token::OpenBrace(..)) => {
                read_token!(self, Token::OpenBrace);

                let initializers = self.parse_initializer_list();

                read_token!(self, Token::CloseBrace);

                AssignmentOrInitializerList::Initializers(initializers)
            }

            _ => AssignmentOrInitializerList::AssignmentExpression(
                self.parse_assignment_expression(),
            ),
        }
    }

    fn parse_initializer_list(&mut self) -> Vec<Initializer> {
        if let Some(Token::CloseBrace(..)) = self.peek() {
            return vec![]
        }

        let mut initializers = vec![self.parse_initializer()];

        while let Some(Token::Comma(..)) = self.peek() {
            read_token!(self, Token::Comma);

            if let Some(Token::CloseBrace(..)) = self.peek() {
                break;
            }

            initializers.push(self.parse_initializer());
        }

        initializers
    }

    fn parse_initializer(&mut self) -> Initializer {
        match self.peek() {
            Some(Token::Dot(..)) => {
                read_token!(self, Token::Dot);
                let ident = read_token!(self, Token::Identifier);
                read_token!(self, Token::Assign);
                let list = self.parse_assignment_or_initializer_list();
                if let Token::Identifier(_, ident) = ident {
                    Initializer::Initializer(Some(ident.clone()), Box::new(list))
                } else {
                    unreachable!()
                }
            }
            _ => Initializer::Initializer(
                None,
                Box::new(self.parse_assignment_or_initializer_list()),
            ),
        }
    }

    fn parse_compound_statement(&mut self) -> CompoundStatement {
        let mut stmts = Vec::new();
        read_token!(self, Token::OpenBrace);
        loop {
            match self.peek() {
                Some(Token::CloseBrace(..)) => break,
                _ => stmts.push(self.parse_stmt_or_declaration()),
            }
        }
        read_token!(self, Token::CloseBrace);
        CompoundStatement::Statements(stmts)
    }

    #[allow(unreachable_patterns)]
    fn parse_stmt_or_declaration(&mut self) -> StatementOrDeclaration {
        if let Some(Token::Identifier(_, ident)) = self.peek() {
            if !self.is_type(&ident.clone()) && !self.is_typeof(ident) {
                return StatementOrDeclaration::Statement(self.parse_statement());
            }
        }
        match_first!(
            self.peek() => _t,
            default unexpected_token!(_t, self),

            Declaration => StatementOrDeclaration::Declaration(self.parse_declaration()),
            Statement => StatementOrDeclaration::Statement(self.parse_statement()),
        )
    }

    fn parse_declaration(&mut self) -> Declaration {
        let spec = self.parse_declaration_specifiers();
        let has_typedef = if let DeclarationSpecifiers::DeclarationSpecifiers(Some(spec), _) = &spec {
            spec.0.typedef
        } else {
            false
        };
        let init_list = match self.peek() {
            Some(Token::Semicolon(..)) => vec![],
            _ => {
                let decl = self.parse_declarator();
                if has_typedef {
                    self.add_type(self.get_decl_identifier(&decl));
                }
                self.parse_init_declarator_list(decl)
            }
        };
        read_token!(self, Token::Semicolon);
        Declaration::Declaration(Box::new(spec), init_list)
    }

    fn parse_statement(&mut self) -> Statement {
        match self.peek_n(2).as_slice() {
            [Token::Identifier(..), Token::Colon(..)] | [Token::Case(..), _] | [Token::Default(..), _]
                => return Statement::LabeledStatement(Box::new(self.parse_labeled_statement())),
            _ => {}
        }
        match_first!(
            self.peek() => _t,
            default unexpected_token!(_t, self),

            AsmStatement => Statement::AsmStatement(Box::new(self.parse_asm_statement())),
            CompoundStatement => Statement::CompoundStatement(Box::new(self.parse_compound_statement())),
            SelectionStatement => Statement::SelectionStatement(Box::new(self.parse_selection_statement())),
            IterationStatement => Statement::IterationStatement(Box::new(self.parse_iteration_statement())),
            JumpStatement => Statement::JumpStatement(Box::new(self.parse_jump_statement())),
            ExpressionStatement => Statement::ExpressionStatement(Box::new(self.parse_expression_statement())),
        )
    }

    fn parse_asm_statement(&mut self) -> AsmStatement {
        read_token!(self, Token::Asm);
        let mut tokens = vec![read_token!(self, Token::OpenParen)];
        let mut depth = 1;
        while let Some(t) = self.next() {
            match &t {
                Token::OpenParen(..) => {
                    tokens.push(t.clone());
                    depth += 1;
                }
                Token::CloseParen(..) => {
                    tokens.push(t.clone());
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                t => { tokens.push((*t).clone()); },
            }
        }
        if depth != 0 {
            panic!("Unexpected end of file");
        }
        AsmStatement::Asm(tokens)
    }

    fn parse_expression_statement(&mut self) -> ExpressionStatement {
        if let Some(Token::Semicolon(..)) = self.peek() {
            read_token!(self, Token::Semicolon);
            ExpressionStatement::Expression(None)
        } else {
            let e = ExpressionStatement::Expression(Some(Box::new(self.parse_expression())));
            read_token!(self, Token::Semicolon);
            e
        }
    }

    fn parse_labeled_statement(&mut self) -> LabeledStatement {
        match self.peek() {
            Some(Token::Identifier(..)) => {
                let ident = read_token!(self, Token::Identifier);
                let c = read_token!(self, Token::Colon);
                let s = self.parse_statement();
                LabeledStatement::Identifier(ident, c, s)
            }
            Some(Token::Case(..)) => {
                let case = read_token!(self, Token::Case);
                let e = self.parse_general_expression();
                if let Some(Token::Varargs(..)) = self.peek() {
                    let v = read_token!(self, Token::Varargs);
                    let e2 = self.parse_general_expression();
                    let c = read_token!(self, Token::Colon);
                    let s = self.parse_statement();
                    LabeledStatement::RangeCase(case, e, v, e2, c, s)
                } else {
                    let c = read_token!(self, Token::Colon);
                    let s = self.parse_statement();
                    LabeledStatement::Case(case, e, c, s)
                }
            }
            Some(Token::Default(..)) => {
                let def = read_token!(self, Token::Default);
                let c = read_token!(self, Token::Colon);
                let s = self.parse_statement();
                LabeledStatement::Default(def, c, s)
            }
            t => unexpected_token!(t, self),
        }
    }

    fn parse_selection_statement(&mut self) -> SelectionStatement {
        match self.peek() {
            Some(Token::If(..)) => {
                read_token!(self, Token::If);
                read_token!(self, Token::OpenParen);
                let e = self.parse_expression();
                read_token!(self, Token::CloseParen);
                let s = self.parse_statement();
                let otherwise = match self.peek() {
                    Some(Token::Else(..)) => {
                        read_token!(self, Token::Else);
                        Some(Box::new(self.parse_statement()))
                    }
                    _ => None
                };
                SelectionStatement::If(Box::new(e), s, otherwise)
            }
            Some(Token::Switch(..)) => {
                read_token!(self, Token::Switch);
                read_token!(self, Token::OpenParen);
                let e = self.parse_expression();
                read_token!(self, Token::CloseParen);
                let s = self.parse_statement();
                SelectionStatement::Switch(Box::new(e), s)
            }
            t => unexpected_token!(t, self),
        }
    }

    fn parse_iteration_statement(&mut self) -> IterationStatement {
        match self.peek() {
            Some(Token::While(..)) => {
                let w = read_token!(self, Token::While);
                let op = read_token!(self, Token::OpenParen);
                let e = Box::new(self.parse_expression());
                let cp = read_token!(self, Token::CloseParen);
                let s = Box::new(self.parse_statement());
                IterationStatement::While(w, op, e, cp, s)
            }
            Some(Token::Do(..)) => {
                let d = read_token!(self, Token::Do);
                let s = Box::new(self.parse_statement());
                let w = read_token!(self, Token::While);
                let op = read_token!(self, Token::OpenParen);
                let e = Box::new(self.parse_expression());
                let cp = read_token!(self, Token::CloseParen);
                let semi = read_token!(self, Token::Semicolon);
                IterationStatement::Do(d, s, w, op, e, cp, semi)
            }
            Some(Token::For(..)) => {
                let f = read_token!(self, Token::For);
                let op = read_token!(self, Token::OpenParen);
                let e1 = Box::new(self.parse_expression_statement());
                let e2 = Box::new(self.parse_expression_statement());
                let f_expr = if let Some(Token::CloseParen(..)) = self.peek() {
                    ForExpr::EmptyLast(e1, e2)
                } else {
                    let e3 = Box::new(self.parse_expression());
                    ForExpr::ExpressionStatement(e1, e2, e3)
                };
                let cp = read_token!(self, Token::CloseParen);
                let s = Box::new(self.parse_statement());
                IterationStatement::For(f, op, f_expr, cp, s)
            }
            t => unexpected_token!(t, self),
        }
    }

    fn parse_jump_statement(&mut self) -> JumpStatement {
        match self.peek() {
            Some(Token::Goto(..)) => {
                let goto = read_token!(self, Token::Goto);
                let ident = read_token!(self, Token::Identifier);
                let semi = read_token!(self, Token::Semicolon);
                JumpStatement::Goto(goto, ident, semi)
            }
            Some(Token::Continue(..)) => {
                let cont = read_token!(self, Token::Continue);
                let semi = read_token!(self, Token::Semicolon);
                JumpStatement::Continue(cont, semi)
            }
            Some(Token::Break(..)) => {
                let br = read_token!(self, Token::Break);
                let semi = read_token!(self, Token::Semicolon);
                JumpStatement::Break(br, semi)
            }
            Some(Token::Return(..)) => {
                let r = read_token!(self, Token::Return);
                if let Some(Token::Semicolon(..)) = self.peek() {
                    let semi = read_token!(self, Token::Semicolon);
                    JumpStatement::ReturnVoid(r, semi)
                } else {
                    let e = self.parse_expression();
                    let semi = read_token!(self, Token::Semicolon);
                    JumpStatement::Return(r, e, semi)
                }
            }
            t => unexpected_token!(t, self),
        }
    }

    fn is_typeof(&self, ident: &str) -> bool {
        match ident {
            "__typeof__" | "typeof" => true,
            _ => false
        }
    }

    fn is_type(&self, ident: &str) -> bool {
        match ident {
            t if self.is_typeof(t) => true,
            "__label__" => true,
            _ => self.scope.iter().any(|e| e.types.contains(ident))
        }
    }

    fn starts_type(&self, t: &Token) -> bool {
        if let Token::Identifier(_, i) = t {
            return self.is_type(i);
        }

        match_first!(
            Some(t) => _t,
            default false,
            DeclarationSpecifiers => true,
        )
    }

    fn parse_expression(&mut self) -> Expression {
        let mut exprs = vec![self.parse_assignment_expression()];
        while let Some(Token::Comma(..)) = self.peek() {
            read_token!(self, Token::Comma);
            exprs.push(self.parse_assignment_expression());
        }
        Expression::Expression(exprs)
    }

    fn parse_assignment_expression(&mut self) -> AssignmentExpression {
        let left = self.parse_ternary_expression();
        match self.peek() {
            Some(Token::Assign(..))
            | Some(Token::TimesAssign(..))
            | Some(Token::DivAssign(..))
            | Some(Token::ModAssign(..))
            | Some(Token::PlusAssign(..))
            | Some(Token::MinusAssign(..))
            | Some(Token::BitShiftLeftAssign(..))
            | Some(Token::BitShiftRightAssign(..))
            | Some(Token::BitAndAssign(..))
            | Some(Token::BitXorAssign(..))
            | Some(Token::BitOrAssign(..)) => {
                if let TernaryExpression::GeneralExpression(GeneralExpression::CastExpression(
                    box CastExpression::UnaryExpression(u),
                )) = left
                {
                    let op = self.next();
                    let right = self.parse_assignment_expression();
                    AssignmentExpression::Assignment(
                        Box::new(u),
                        self.assignment_operator(op.unwrap().clone()),
                        Box::new(right),
                    )
                } else {
                    panic!("Tried to assign to a rvalue")
                }
            }
            _ => AssignmentExpression::TernaryExpression(left),
        }
    }
    fn parse_ternary_expression(&mut self) -> TernaryExpression {
        let e = self.parse_general_expression();
        if let Some(Token::Ternary(..)) = self.peek() {
            let t1 = read_token!(self, Token::Ternary);
            let if_true = self.parse_expression();
            let t2 = read_token!(self, Token::Colon);
            let otherwise = self.parse_ternary_expression();
            TernaryExpression::Ternary(*e, t1, Box::new(if_true), t2, Box::new(otherwise))
        } else {
            TernaryExpression::GeneralExpression(*e)
        }
    }

    fn parse_general_expression(&mut self) -> Box<GeneralExpression> {
        let left = self.parse_cast_expression();
        self.parse_general_expression_(
            1,
            Box::new(GeneralExpression::CastExpression(Box::new(left))),
        )
    }

    #[allow(clippy::cognitive_complexity)]
    #[rustfmt::skip]
    fn parse_general_expression_(
        &mut self,
        priority: usize,
        mut left: Box<GeneralExpression>,
    ) -> Box<GeneralExpression> {
        loop {
            match self.peek() {
                Some(Token::Times(..)) if priority == 12 => operation_left!(left self Times),
                Some(Token::Times(..)) if priority < 12 => operation_right!(left self 12),
                Some(Token::Divide(..)) if priority == 12 => operation_left!(left self Divide),
                Some(Token::Divide(..)) if priority < 12 => operation_right!(left self 12),
                Some(Token::Mod(..)) if priority == 12 => operation_left!(left self Mod),
                Some(Token::Mod(..)) if priority < 12 => operation_right!(left self 12),
                Some(Token::Plus(..)) if priority == 11 => operation_left!(left self Plus),
                Some(Token::Plus(..)) if priority < 11 => operation_right!(left self 11),
                Some(Token::Minus(..)) if priority == 11 => operation_left!(left self Minus),
                Some(Token::Minus(..)) if priority < 11 => operation_right!(left self 11),
                Some(Token::BitShiftLeft(..)) if priority == 10 => operation_left!(left self BitShiftLeft),
                Some(Token::BitShiftLeft(..)) if priority < 10 => operation_right!(left self 10),
                Some(Token::BitShiftRight(..)) if priority == 10 => operation_left!(left self BitShiftRight),
                Some(Token::BitShiftRight(..)) if priority < 10 => operation_right!(left self 10),
                Some(Token::LessThan(..)) if priority == 9 => operation_left!(left self LessThan),
                Some(Token::LessThan(..)) if priority < 9 => operation_right!(left self 9),
                Some(Token::MoreThan(..)) if priority == 9 => operation_left!(left self MoreThan),
                Some(Token::MoreThan(..)) if priority < 9 => operation_right!(left self 9),
                Some(Token::LessEqThan(..)) if priority == 9 => operation_left!(left self LessEqThan),
                Some(Token::LessEqThan(..)) if priority < 9 => operation_right!(left self 9),
                Some(Token::MoreEqThan(..)) if priority == 9 => operation_left!(left self MoreEqThan),
                Some(Token::MoreEqThan(..)) if priority < 9 => operation_right!(left self 9),
                Some(Token::Equals(..)) if priority == 8 => operation_left!(left self Equals),
                Some(Token::Equals(..)) if priority < 8 => operation_right!(left self 8),
                Some(Token::NotEquals(..)) if priority == 8 => operation_left!(left self NotEquals),
                Some(Token::NotEquals(..)) if priority < 8 => operation_right!(left self 8),
                Some(Token::BitAnd(..)) if priority == 7 => operation_left!(left self BitAnd),
                Some(Token::BitAnd(..)) if priority < 7 => operation_right!(left self 7),
                Some(Token::BitXor(..)) if priority == 6 => operation_left!(left self BitXor),
                Some(Token::BitXor(..)) if priority < 6 => operation_right!(left self 6),
                Some(Token::BitOr(..)) if priority == 5 => operation_left!(left self BitOr),
                Some(Token::BitOr(..)) if priority < 5 => operation_right!(left self 5),
                Some(Token::And(..)) if priority == 4 => operation_left!(left self And),
                Some(Token::And(..)) if priority < 4 => operation_right!(left self 4),
                Some(Token::Or(..)) if priority == 3 => operation_left!(left self Or),
                Some(Token::Or(..)) if priority < 3 => operation_right!(left self 3),
                _ => break,
            }
        }
        left
    }

    fn starts_struct_literal(&mut self) -> bool {
        let mut parens = 0;
        self.peek(); // Clear out peek buf
        loop {
            match self.iter.peek_and_advance() {
                Some(Token::OpenParen(..)) => parens += 1,
                Some(Token::OpenBrace(..)) => parens += 1,
                Some(Token::OpenBracket(..)) => parens += 1,
                Some(Token::CloseParen(..)) => parens -= 1,
                Some(Token::CloseBrace(..)) => parens -= 1,
                Some(Token::CloseBracket(..)) => parens -= 1,
                Some(_) => {}
                t => unexpected_token!(t, self),
            }
            if parens == 0 {
                break;
            }
        }

        if let Some(Token::OpenBrace(..)) = self.iter.peek_buf() {
            true
        } else {
            false
        }
    }

    fn parse_cast_expression(&mut self) -> CastExpression {
        if let [Token::OpenParen(..), t] = self.peek_n(2).as_slice() {
            let tt = *t.clone();
            if self.starts_type(&tt) && !self.starts_struct_literal() {
                let op = read_token!(self, Token::OpenParen);
                let ty = self.parse_type_name();
                let cp = read_token!(self, Token::CloseParen);
                let e = self.parse_general_expression();
                return CastExpression::OpenParen(op, Box::new(ty), cp, e);
            }
        }
        CastExpression::UnaryExpression(self.parse_unary_expression())
    }

    fn parse_unary_expression(&mut self) -> UnaryExpression {
        match self.peek() {
            Some(Token::Increment(..)) | Some(Token::Decrement(..)) => {
                let t = self.next().cloned().unwrap();
                let op = self.increment_or_decrement(t);
                UnaryExpression::IncrementOrDecrement(op, Box::new(self.parse_unary_expression()))
            }
            Some(Token::BitAnd(..))
            | Some(Token::Times(..))
            | Some(Token::Plus(..))
            | Some(Token::Minus(..))
            | Some(Token::BitNot(..))
            | Some(Token::Not(..)) => {
                let t = self.next().cloned().unwrap();
                let op = self.unary_operator(t);
                UnaryExpression::UnaryOperator(op, Box::new(self.parse_cast_expression()))
            }
            Some(Token::Sizeof(..)) => {
                read_token!(self, Token::Sizeof);
                if let [Token::OpenParen(..), t] = self.peek_n(2).as_slice() {
                    let tt = *t.clone();
                    if self.starts_type(tt) {
                        read_token!(self, Token::OpenParen);
                        let ty = self.parse_type_name();
                        read_token!(self, Token::CloseParen);
                        return UnaryExpression::SizeofTy(Box::new(ty));
                    }
                }
                let e = self.parse_unary_expression();
                UnaryExpression::SizeofExpr(Box::new(e))
            }
            /* GNU extension */
            Some(Token::And(..)) => {
                read_token!(self, Token::And);
                let ident = read_token!(self, Token::Identifier).get_ident_str().clone();
                UnaryExpression::AddressOfLabel(ident)
            }
            _ => UnaryExpression::PostfixExpression(Box::new(self.parse_postfix_expression())),
        }
    }

    fn parse_postfix_expression(&mut self) -> PostfixExpression {
        let mut e = PostfixExpression::PrimaryExpression(self.parse_primary_expression());
        loop {
            match self.peek() {
                Some(Token::OpenBracket(..)) => {
                    let op = read_token!(self, Token::OpenBracket);
                    let expr = self.parse_expression();
                    let cp = read_token!(self, Token::CloseBracket);
                    e = PostfixExpression::Index(Box::new(e), op, Box::new(expr), cp);
                }
                Some(Token::Dot(..)) => {
                    let dot = read_token!(self, Token::Dot);
                    let ident = read_token!(self, Token::Identifier);
                    e = PostfixExpression::Member(Box::new(e), MemberAccess::Dot(dot), ident);
                }
                Some(Token::Arrow(..)) => {
                    let arrow = read_token!(self, Token::Arrow);
                    let ident = read_token!(self, Token::Identifier);
                    e = PostfixExpression::Member(Box::new(e), MemberAccess::Arrow(arrow), ident);
                }
                Some(Token::OpenParen(..)) => {
                    let op = read_token!(self, Token::OpenParen);
                    let mut list = Vec::new();
                    loop {
                        if let Some(Token::CloseParen(..)) = self.peek() {
                            break;
                        } else {
                            list.push(self.parse_assignment_expression());
                            if let Some(Token::Comma(..)) = self.peek() {
                                read_token!(self, Token::Comma);
                            } else {
                                break;
                            }
                        }
                    }
                    let cp = read_token!(self, Token::CloseParen);
                    e = PostfixExpression::Call(Box::new(e), op, ArgumentExpressionList::List(list), cp);
                }
                Some(Token::Increment(..)) | Some(Token::Decrement(..)) => {
                    let t = self.next().cloned().unwrap();
                    let inc = self.increment_or_decrement(t);
                    e = PostfixExpression::Increment(Box::new(e), inc);
                }
                _ => return e
            }
        }
    }

    fn parse_primary_expression(&mut self) -> PrimaryExpression {
        match self.peek() {
            Some(Token::Identifier(..)) => {
                PrimaryExpression::Identifier(self.next().unwrap().get_ident_str().clone())
            }
            Some(Token::Number(..)) => PrimaryExpression::Number(self.next().unwrap().get_ident_str().clone()),
            Some(Token::StringLiteral(..)) => {
                PrimaryExpression::StringLiteral(self.next().unwrap().get_ident_str().clone())
            }
            Some(Token::CharLiteral(..)) => {
                PrimaryExpression::CharLiteral(self.next().unwrap().get_char_val())
            }
            Some(Token::OpenParen(..)) => {
                read_token!(self, Token::OpenParen);
                if let Some(Token::OpenBrace(..)) = self.peek() {
                    let compound = self.parse_compound_statement();
                    read_token!(self, Token::CloseParen);
                    PrimaryExpression::Statement(Box::new(compound))
                } else if self.peek().cloned().map(|t| self.starts_type(t)) == Some(true) {
                    let ty = self.parse_type_name();
                    read_token!(self, Token::CloseParen);
                    read_token!(self, Token::OpenBrace);
                    let initializers = self.parse_initializer_list();
                    read_token!(self, Token::CloseBrace);
                    PrimaryExpression::StructValue(Box::new(ty), initializers)
                } else {
                    let e = Box::new(self.parse_expression());
                    read_token!(self, Token::CloseParen);
                    PrimaryExpression::Expression(e)
                }
            }
            t => unexpected_token!(t, self),
        }
    }

    fn assignment_operator(&self, token: Token) -> AssignmentOperator {
        match token {
            Token::Assign(..) => AssignmentOperator::Assign(token),
            Token::TimesAssign(..) => AssignmentOperator::TimesAssign(token),
            Token::DivAssign(..) => AssignmentOperator::DivAssign(token),
            Token::ModAssign(..) => AssignmentOperator::ModAssign(token),
            Token::PlusAssign(..) => AssignmentOperator::PlusAssign(token),
            Token::MinusAssign(..) => AssignmentOperator::MinusAssign(token),
            Token::BitShiftLeftAssign(..) => AssignmentOperator::BitShiftLeftAssign(token),
            Token::BitShiftRightAssign(..) => AssignmentOperator::BitShiftRightAssign(token),
            Token::BitAndAssign(..) => AssignmentOperator::BitAndAssign(token),
            Token::BitXorAssign(..) => AssignmentOperator::BitXorAssign(token),
            Token::BitOrAssign(..) => AssignmentOperator::BitOrAssign(token),
            _ => unreachable!(),
        }
    }

    fn increment_or_decrement(&self, token: Token) -> IncrementOrDecrement {
        match token {
            Token::Increment(..) => IncrementOrDecrement::Increment(token),
            Token::Decrement(..) => IncrementOrDecrement::Decrement(token),
            _ => unreachable!(),
        }
    }

    fn unary_operator(&self, token: Token) -> UnaryOperator {
        match token {
            Token::BitAnd(..) => UnaryOperator::BitAnd(token),
            Token::Times(..) => UnaryOperator::Times(token),
            Token::Plus(..) => UnaryOperator::Plus(token),
            Token::Minus(..) => UnaryOperator::Minus(token),
            Token::BitNot(..) => UnaryOperator::BitNot(token),
            Token::Not(..) => UnaryOperator::Not(token),
            _ => unreachable!(),
        }
    }

    pub fn ext_decl_identifiers(&self, ext_decl: &ExternalDeclaration) -> Vec<Rc<str>> {
        if let ExternalDeclaration::Declaration(box Declaration::Declaration(_, list)) = ext_decl {
            list.iter().map(|e| {
                match e {
                    InitDeclarator::Declarator(decl) => self.get_decl_identifier(&**decl),
                    _ => panic!()
                }
            }).collect()
        } else {
            vec![]
        }
    }

    fn get_decl_identifier(&self, decl: &Declarator) -> Rc<str> {
        match decl {
            Declarator::Declarator(_, direct_decl) => self.get_direct_decl_identifier(direct_decl),
        }
    }

    fn get_direct_decl_identifier(&self, direct_decl: &DirectDeclarator) -> Rc<str> {
        match direct_decl {
            DirectDeclarator::Identifier(ident) => ident.clone(),
            DirectDeclarator::Parens(decl) => self.get_decl_identifier(decl),
            DirectDeclarator::Array(direct_decl, _) => self.get_direct_decl_identifier(direct_decl),
            DirectDeclarator::Function(direct_decl, _) => self.get_direct_decl_identifier(direct_decl),
        }
    }
}

trait ReplaceEmpty<T> {
    fn replace_if_empty(&mut self, t: T, e: &str);
}

impl<T> ReplaceEmpty<T> for Option<T>
where
    T: Debug,
{
    fn replace_if_empty(&mut self, t: T, e: &str) {
        if self.is_some() {
            panic!(
                "{} (tried to replace {:?} with {:?})",
                e,
                self.as_ref().unwrap(),
                t
            );
        }

        self.replace(t);
    }
}
