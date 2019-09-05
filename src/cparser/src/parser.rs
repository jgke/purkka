use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt::Debug;
use std::rc::Rc;

use debug::debug::{if_debug, DebugVal};
use shared::traits::{multipeek, MultiPeek};
use shared::utils;

use ctoken::token::Token;
use purkkasyntax::{Primitive, TypeSignature};
use resolve::Declarations;

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

macro_rules! read_identifier_str {
    ($iter:expr) => {
        if let Token::Identifier(_, s) = read_token!($iter, Token::Identifier) {
            s
        } else {
            unreachable!()
        }
    };
}

macro_rules! operation_left {
    ($left:ident, $val:ident, $ty:ident, $this:ident, $target:ident, $op:tt) => {{
        let t = $this.next().cloned().unwrap();
        let (e, val, ty) = $this.parse_cast_expression();
        $left = Box::new(GeneralExpression::$target(
            $left,
            t,
            Box::new(GeneralExpression::CastExpression(Box::new(e))),
        ));
        $val = $val.and_then(|v1| val.clone().map(|v2| v1 $op v2));
        $ty = $ty.and_then(|v1| ty.clone().map(|_| v1));
    }};
}

macro_rules! operation_right {
    ($left:ident, $val:ident, $ty:ident, $this:ident, $prio:literal) => {{
        let e = $this.parse_general_expression_($prio, $left, $val, $ty);
        $left = e.0;
        $val = e.1;
        $ty = e.2;
    }};
}

macro_rules! operation_left_b {
    ($left:ident, $val:ident, $ty:ident, $this:ident, $target:ident, $op:tt) => {{
        let t = $this.next().cloned().unwrap();
        let (e, val, _ty) = $this.parse_cast_expression();
        $left = Box::new(GeneralExpression::$target(
            $left,
            t,
            Box::new(GeneralExpression::CastExpression(Box::new(e))),
        ));
        $val = $val.and_then(|v1| val.map(|v2| if (v1 != 0) $op (v2 != 0) { 1 } else { 0 }));
        $ty = Some(TypeSignature::int());
    }};
}

macro_rules! op_table {
    ($left:ident, $val:ident, $ty:ident, $priority_t:ident, $this:ident,
     $($row:tt)*) => {
        op_table!(@ $left, $val, $ty, $priority_t, $this, {}, [$($row)*] )
    };

    (@ $left:ident, $val:ident, $ty:ident, $priority_t:ident, $this:ident,
     {$($arms:tt)*}, []) => {
        match $this.peek() {
            $($arms)*
            _ => break
        }
    };

    (@ $left:ident, $val:ident, $ty:ident, $priority_t:ident, $this:ident,
     {$($arms:tt)*},
     [(num, $target:ident, $priority:literal, $op:tt) $($rest:tt)*]) => {
        op_table!(@ $left, $val, $ty, $priority_t, $this, {
            $($arms)*
            Some(Token::$target(..)) if $priority_t == $priority =>
                operation_left!($left, $val, $ty, $this, $target, $op),
            Some(Token::$target(..)) if $priority_t < $priority =>
                operation_right!($left, $val, $ty, $this, $priority),
        }, [$($rest)*] )
    };

    (@ $left:ident, $val:ident, $ty:ident, $priority_t:ident, $this:ident,
     {$($arms:tt)*},
     [(bool, $target:ident, $priority:literal, $op:tt) $($rest:tt)*]) => {
        op_table!(@ $left, $val, $ty, $priority_t, $this, {
            Some(Token::$target(..)) if $priority_t == $priority =>
                operation_left_b!($left, $val, $ty, $this, $target, $op),
            Some(Token::$target(..)) if $priority_t < $priority =>
                operation_right!($left, $val, $ty, $this, $priority),
            $($arms)*
        }, [$($rest)*] )
    };
}

fn has_typedef(spec: &DeclarationSpecifiers) -> bool {
    let DeclarationSpecifiers::DeclarationSpecifiers(spec, _) = spec;
    spec.0.typedef
}

#[derive(Debug, Default)]
pub struct ScopedState {
    pub types: HashSet<Rc<str>>,
    pub labels: HashSet<Rc<str>>,
    pub idents: HashMap<Rc<str>, (DeclarationSpecifiers, Declarator)>,
}

#[derive(Debug)]
pub struct ParseContext<'a, I>
where
    I: Iterator<Item = &'a Token>,
{
    pub scope: Vec<ScopedState>,
    pub vals: HashMap<Rc<str>, i128>,
    pub iter: MultiPeek<I>,
    pub fragment: &'a FragmentIterator,
    pub sources: &'a [Source],
}

fn default_types(mut types: HashSet<Rc<str>>) -> ScopedState {
    types.insert(From::from("__builtin_va_list"));
    types.insert(From::from("_Bool"));
    types.insert(From::from("_Complex"));

    let mut state = ScopedState::default();
    state.types = types;
    state
}

pub fn parse<'a, I>(
    iter: I,
    sources: &'a [Source],
    fragment_iter: &'a FragmentIterator,
    types: HashSet<Rc<str>>,
) -> Result<S, Option<Token>>
where
    I: IntoIterator<Item = &'a Token>,
{
    let context = ParseContext {
        scope: vec![default_types(types)],
        vals: HashMap::new(),
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

pub fn parse_macro_expansion<'a, I>(
    iter: I,
    sources: &'a [Source],
    fragment_iter: &'a FragmentIterator,
    types: HashSet<Rc<str>>,
) -> Result<Vec<MacroExpansion>, Option<Token>>
where
    I: IntoIterator<Item = &'a Token>,
{
    if_debug(DebugVal::CParserToken, || {
        println!();
    });
    let context = ParseContext {
        scope: vec![default_types(types)],
        vals: HashMap::new(),
        iter: multipeek(iter),
        fragment: fragment_iter,
        sources,
    };
    let context_mut = std::sync::Mutex::new(context);

    std::panic::catch_unwind(|| {
        context_mut
            .lock()
            .unwrap()
            .parse_type_statement_or_expression()
    })
    .map_err(|_| match context_mut.lock() {
        Ok(mut context) => context.iter.peek().cloned().cloned(),
        Err(context) => context.into_inner().iter.peek().cloned().cloned(),
    })
}

impl<'a, I> ParseContext<'a, I>
where
    I: Iterator<Item = &'a Token>,
{
    fn next(&mut self) -> Option<&'a Token> {
        if_debug(DebugVal::CParserToken, || {
            if let Some(Token::Identifier(_, i)) = self.peek() {
                println!("is {} type: {:?}", i, self.is_type(i));
            } else {
                println!("{:?}", self.peek());
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
    fn get_val(&mut self, key: &str) -> Option<i128> {
        self.vals.get(key).copied()
    }
    fn put_val(&mut self, key: Rc<str>, val: i128) {
        self.vals.insert(key, val);
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
        let has_typedef = has_typedef(&spec);
        if let Some(Token::Semicolon(..)) = self.peek() {
            return ExternalDeclaration::Declaration(Box::new(Declaration::Declaration(
                Box::new(spec),
                vec![],
                None,
            )));
        }

        let attrs = self.maybe_parse_attributes();
        assert!(attrs.is_empty());

        let declarator = self.parse_declarator();

        let ext_decl = match self.peek() {
            Some(Token::OpenBrace(..)) => {
                let block = Box::new(self.parse_compound_statement());
                return ExternalDeclaration::FunctionDefinition(Box::new(
                    FunctionDefinition::FunctionDefinition(
                        Some(Box::new(spec)),
                        Box::new(declarator),
                        block,
                    ),
                ));
            }
            Some(Token::Semicolon(..)) => {
                ExternalDeclaration::Declaration(Box::new(Declaration::Declaration(
                    Box::new(spec),
                    vec![InitDeclarator::Declarator(Box::new(declarator))],
                    None,
                )))
            }
            _ => {
                let init_list = self.parse_init_declarator_list(declarator);
                let attrs = self.maybe_parse_attributes();
                ExternalDeclaration::Declaration(Box::new(Declaration::Declaration(
                    Box::new(spec),
                    init_list,
                    Some(attrs),
                )))
            }
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
        let abs = match self.peek() {
            Some(Token::OpenParen(..)) | Some(Token::OpenBracket(..)) => {
                self.parse_direct_abstract_declarator()
            }
            _ => Box::new(DirectAbstractDeclarator::Epsilon()),
        };
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
                        _ => Some(self.parse_general_expression().0),
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

        DeclarationSpecifiers::DeclarationSpecifiers(spec, ty)
    }

    fn parse_specifiers(&mut self, spec: &mut Specifiers) {
        loop {
            self.maybe_parse_attributes();
            match self.peek() {
                Some(Token::Typedef(..)) => spec.0.typedef = true,
                Some(Token::Extern(..)) => spec.0.extern_ = true,
                Some(Token::Static(..)) => spec.0.static_ = true,
                Some(Token::Inline(..)) => spec.0.inline = true,
                Some(Token::Auto(..)) => spec.0.auto = true,
                Some(Token::Register(..)) => spec.0.register = true,
                Some(Token::Const(..)) => spec.1.const_ = true,
                Some(Token::Volatile(..)) => spec.1.volatile = true,
                Some(Token::Restrict(..)) => spec.1.restrict = true,
                _ => break,
            }
            self.next();
        }
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_type_specifier(&mut self) -> Option<TypeSpecifier> {
        let mut longs = 0;
        let mut complex = None;
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
                    if data != Some(PrimitiveType::Short) && data != Some(PrimitiveType::Long) {
                        data.replace_if_empty(PrimitiveType::Int, "Conflicting type specifiers");
                    }
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
                Some(Token::Identifier(_, ident))
                    if ident.as_ref() == "__float128" || ident.as_ref() == "__Float128" =>
                {
                    read_token!(self, Token::Identifier);
                    data.replace_if_empty(PrimitiveType::Double, "Conflicting type specifiers");
                }
                Some(Token::Identifier(_, ident)) if ident.as_ref() == "_Complex" => {
                    read_token!(self, Token::Identifier);
                    complex.replace_if_empty(Some(()), "_Complex specified twice");
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

        if longs == 0 && sign.is_none() && data.is_none() && ty.is_none() && complex.is_none() {
            None
        } else if longs == 0
            && sign.is_none()
            && data.is_none()
            && ty.is_none()
            && complex.is_some()
        {
            Some(CType::Complex(None, PrimitiveType::Double))
        } else if (longs != 0 || sign.is_some() || data.is_some() || complex.is_some())
            && ty.is_some()
        {
            panic!("Conflicting type specifiers");
        } else if ty.is_some() {
            ty
        } else {
            let (sign, data) = match (longs, sign, data) {
                (0, sign, Some(primitive)) => (sign, primitive),
                (0, sign, None) => (sign, PrimitiveType::Int),

                (1, sign, Some(PrimitiveType::Int)) | (1, sign, None) => {
                    (sign, PrimitiveType::Long)
                }
                (2, sign, Some(PrimitiveType::Int)) | (2, sign, None) => {
                    (sign, PrimitiveType::LongLong)
                }

                (_, Some(_), Some(PrimitiveType::Float))
                | (_, Some(_), Some(PrimitiveType::Double)) => {
                    panic!("Signed floats are not supported")
                }
                (1, None, Some(PrimitiveType::Double)) => (None, PrimitiveType::LongDouble),

                (t, _, _) if t > 2 => panic!("Type is too long! ({})", t),
                _ => unreachable!(),
            };

            if complex.is_some() {
                Some(CType::Complex(sign, data))
            } else {
                Some(CType::Primitive(sign, data))
            }
        }
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_compound_type(&mut self) -> CompoundType {
        let mut field_index = 0;
        if let Some(Token::Enum(..)) = self.peek() {
            read_token!(self, Token::Enum);
            let ident = maybe_read_token!(self, Token::Identifier);
            match self.peek() {
                Some(Token::OpenBrace(..)) => {
                    read_token!(self, Token::OpenBrace);
                    let mut fields = vec![self.parse_enum_field(&mut field_index)];
                    while let Some(Token::Comma(..)) = self.peek() {
                        read_token!(self, Token::Comma);
                        if let Some(Token::CloseBrace(..)) = self.peek() {
                            break;
                        }
                        fields.push(self.parse_enum_field(&mut field_index));
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
                                self.maybe_parse_attributes();
                                read_token!(self, Token::Semicolon);
                            }
                        }
                    }
                    read_token!(self, Token::CloseBrace);

                    match ident {
                        Some(t) => {
                            if is_union {
                                CompoundType::Union(t.get_ident_str().clone(), Some(fields))
                            } else {
                                CompoundType::Struct(t.get_ident_str().clone(), Some(fields))
                            }
                        }
                        None => {
                            if is_union {
                                CompoundType::AnonymousUnion(fields)
                            } else {
                                CompoundType::AnonymousStruct(fields)
                            }
                        }
                    }
                }
                _ => match ident {
                    Some(t) => {
                        if is_union {
                            CompoundType::Union(t.get_ident_str().clone(), None)
                        } else {
                            CompoundType::Struct(t.get_ident_str().clone(), None)
                        }
                    }
                    None => unexpected_token!(self.peek(), self),
                },
            }
        }
    }

    fn parse_enum_field(
        &mut self,
        field_index: &mut i128,
    ) -> (Rc<str>, Option<TernaryExpression>, i128) {
        let ident = read_identifier_str!(self);
        let res = if let Some(Token::Assign(..)) = self.peek() {
            read_token!(self, Token::Assign);
            let (expr, val, _) = self.parse_ternary_expression();
            *field_index = val.unwrap();
            self.put_val(ident.clone(), *field_index);
            (ident, Some(expr), *field_index)
        } else {
            self.put_val(ident.clone(), *field_index);
            (ident, None, *field_index)
        };

        *field_index += 1;
        res
    }

    fn parse_struct_field(&mut self) -> StructField {
        let specifiers = Box::new(self.parse_declaration_specifiers());
        match self.peek() {
            Some(Token::Semicolon(..)) => (specifiers, Vec::new()),
            _ => {
                let either = self.parse_declarator_or_abstract_declarator();
                let expr = match self.peek() {
                    Some(Token::Colon(..)) => {
                        read_token!(self, Token::Colon);
                        Some(self.parse_general_expression().0)
                    }
                    _ => None,
                };
                let mut decls = vec![(either, expr)];
                while let Some(Token::Comma(..)) = self.peek() {
                    read_token!(self, Token::Comma);
                    let either = self.parse_declarator_or_abstract_declarator();
                    let expr = match self.peek() {
                        Some(Token::Colon(..)) => {
                            read_token!(self, Token::Colon);
                            Some(self.parse_general_expression().0)
                        }
                        _ => None,
                    };
                    decls.push((either, expr));
                }
                (specifiers, decls)
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
    fn declarator_upcoming(&mut self) -> bool {
        self.peek_pointer();
        self.direct_declarator_upcoming()
    }

    /* Consume a pointer from the peek buf */
    fn peek_pointer(&mut self) {
        if let Some(Token::Times(..)) = self.iter.peek_buf() {
            self.iter.peek_and_advance();
            loop {
                match self.iter.peek_buf() {
                    Some(Token::Volatile(..))
                    | Some(Token::Const(..))
                    | Some(Token::Restrict(..)) => self.iter.peek_and_advance(),
                    _ => break,
                };
            }
            self.peek_pointer();
        }
    }

    fn direct_declarator_upcoming(&mut self) -> bool {
        match self.iter.peek_and_advance() {
            Some(Token::Identifier(..)) => true,
            Some(Token::OpenParen(..)) => self.declarator_upcoming(),
            _ => false,
        }
    }

    fn parse_declarator_or_abstract_declarator(&mut self) -> EitherDeclarator {
        self.peek(); /* Clear out peek buf, just in case */
        if self.declarator_upcoming() {
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
                        Some(Token::Restrict(..)) => spec.restrict = true,
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
        let mut decl = match self.peek() {
            Some(Token::Identifier(..)) => {
                Box::new(DirectDeclarator::Identifier(read_identifier_str!(self)))
            }
            Some(Token::Asm(..)) => Box::new(DirectDeclarator::AsmStatement(Box::new(
                self.parse_asm_statement(),
            ))),
            _ => {
                read_token!(self, Token::OpenParen);
                let d = self.parse_declarator();
                read_token!(self, Token::CloseParen);
                Box::new(DirectDeclarator::Parens(Box::new(d)))
            }
        };
        loop {
            match self.peek() {
                Some(Token::OpenBracket(..)) => {
                    read_token!(self, Token::OpenBracket);
                    let expr = match self.peek() {
                        Some(Token::CloseBracket(..)) => None,
                        _ => Some(self.parse_general_expression().0),
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
                        Some(Token::Comma(..)) | Some(Token::CloseParen(..)) => {
                            if spec
                                == Box::new(DeclarationSpecifiers::DeclarationSpecifiers(
                                    Specifiers::default(),
                                    Some(CType::Void),
                                ))
                            {
                                continue;
                            }
                            params.push(FunctionParam::Parameter(
                                ParameterDeclaration::DeclarationSpecifiers(spec),
                            ))
                        }
                        _ => {
                            let decl = match self.parse_declarator_or_abstract_declarator() {
                                EitherDeclarator::Anonymous(declarator) => {
                                    ParameterDeclaration::AbstractDeclarator(
                                        spec,
                                        Box::new(declarator),
                                    )
                                }
                                EitherDeclarator::Declarator(declarator) => {
                                    ParameterDeclaration::Declarator(spec, Box::new(declarator))
                                }
                            };
                            if decl
                                == ParameterDeclaration::AbstractDeclarator(
                                    Box::new(DeclarationSpecifiers::DeclarationSpecifiers(
                                        Specifiers::default(),
                                        Some(CType::Void),
                                    )),
                                    Box::new(AbstractDeclarator::AbstractDeclarator(
                                        None,
                                        Box::new(DirectAbstractDeclarator::Epsilon()),
                                    )),
                                )
                            {
                                continue;
                            }
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
                let statement = self.parse_asm_statement();
                InitDeclarator::Asm(Box::new(decl), Box::new(statement))
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
                self.parse_assignment_expression().0,
            ),
        }
    }

    fn parse_initializer_list(&mut self) -> Vec<Initializer> {
        if let Some(Token::CloseBrace(..)) = self.peek() {
            return vec![];
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
                return StatementOrDeclaration::Statement(self.parse_statement(true));
            }
        }
        match_first!(
            self.peek() => _t,
            default unexpected_token!(_t, self),

            Declaration => StatementOrDeclaration::Declaration(self.parse_declaration()),
            Statement => StatementOrDeclaration::Statement(self.parse_statement(true)),
        )
    }

    fn parse_declaration(&mut self) -> Declaration {
        let spec = self.parse_declaration_specifiers();
        let has_typedef = has_typedef(&spec);
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
        let attrs = self.maybe_parse_attributes();
        assert!(attrs.is_empty());
        read_token!(self, Token::Semicolon);
        Declaration::Declaration(Box::new(spec), init_list, None)
    }

    fn parse_statement(&mut self, semicolon: bool) -> Statement {
        match self.peek_n(2).as_slice() {
            [Token::Identifier(..), Token::Colon(..)]
            | [Token::Case(..), _]
            | [Token::Default(..), _] => {
                return Statement::LabeledStatement(Box::new(self.parse_labeled_statement(semicolon)))
            }
            _ => {}
        }
        match_first!(
            self.peek() => _t,
            default unexpected_token!(_t, self),

            AsmStatement => {
                let res = Statement::AsmStatement(Box::new(self.parse_asm_statement()));
                if semicolon {
                    read_token!(self, Token::Semicolon);
                }
                res
            },
            CompoundStatement => Statement::CompoundStatement(Box::new(self.parse_compound_statement())),
            SelectionStatement => Statement::SelectionStatement(Box::new(self.parse_selection_statement())),
            IterationStatement => Statement::IterationStatement(Box::new(self.parse_iteration_statement())),
            JumpStatement => Statement::JumpStatement(Box::new(self.parse_jump_statement())),
            ExpressionStatement => Statement::ExpressionStatement(Box::new(self.parse_expression_statement(semicolon))),
        )
    }

    fn parse_asm_statement(&mut self) -> AsmStatement {
        read_token!(self, Token::Asm);

        let mut tokens = vec![];
        let mut volatile = false;
        let mut goto = false;
        let mut inline = false;

        loop {
            match self.next() {
                Some(Token::Volatile(..)) => volatile = true,
                Some(Token::Goto(..)) => goto = true,
                Some(Token::Inline(..)) => inline = true,
                Some(t @ Token::OpenParen(..)) => {
                    tokens.push((*t).clone());
                    break;
                }
                t => unexpected_token!(t, self),
            }
        }

        let mut depth = 1;

        while let Some(t) = self.next() {
            match &t {
                Token::OpenParen(..) => {
                    tokens.push((*t).clone());
                    depth += 1;
                }
                Token::CloseParen(..) => {
                    tokens.push((*t).clone());
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                t => {
                    tokens.push((*t).clone());
                }
            }
        }

        if depth != 0 {
            panic!("Unexpected end of file");
        }

        AsmStatement::Asm {
            tokens,
            volatile,
            goto,
            inline,
        }
    }

    fn parse_expression_statement(&mut self, semicolon: bool) -> ExpressionStatement {
        if let Some(Token::Semicolon(..)) = self.peek() {
            if semicolon {
                read_token!(self, Token::Semicolon);
            }
            ExpressionStatement::Expression(None)
        } else {
            let e = ExpressionStatement::Expression(Some(Box::new(self.parse_expression().0)));
            if semicolon {
                read_token!(self, Token::Semicolon);
            }
            e
        }
    }

    fn parse_labeled_statement(&mut self, semicolon: bool) -> LabeledStatement {
        match self.peek() {
            Some(Token::Identifier(..)) => {
                let ident = read_token!(self, Token::Identifier);
                let c = read_token!(self, Token::Colon);
                let s = self.parse_statement(semicolon);
                LabeledStatement::Identifier(ident, c, s)
            }
            Some(Token::Case(..)) => {
                let case = read_token!(self, Token::Case);
                let e = self.parse_general_expression().0;
                if let Some(Token::Varargs(..)) = self.peek() {
                    let v = read_token!(self, Token::Varargs);
                    let e2 = self.parse_general_expression().0;
                    let c = read_token!(self, Token::Colon);
                    let s = self.parse_statement(semicolon);
                    LabeledStatement::RangeCase(case, e, v, e2, c, s)
                } else {
                    let c = read_token!(self, Token::Colon);
                    let s = self.parse_statement(semicolon);
                    LabeledStatement::Case(case, e, c, s)
                }
            }
            Some(Token::Default(..)) => {
                let def = read_token!(self, Token::Default);
                let c = read_token!(self, Token::Colon);
                let s = self.parse_statement(semicolon);
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
                let e = self.parse_expression().0;
                read_token!(self, Token::CloseParen);
                let s = self.parse_statement(true);
                let otherwise = match self.peek() {
                    Some(Token::Else(..)) => {
                        read_token!(self, Token::Else);
                        Some(Box::new(self.parse_statement(true)))
                    }
                    _ => None,
                };
                SelectionStatement::If(Box::new(e), s, otherwise)
            }
            Some(Token::Switch(..)) => {
                read_token!(self, Token::Switch);
                read_token!(self, Token::OpenParen);
                let e = self.parse_expression().0;
                read_token!(self, Token::CloseParen);
                let s = self.parse_statement(true);
                SelectionStatement::Switch(Box::new(e), s)
            }
            t => unexpected_token!(t, self),
        }
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_iteration_statement(&mut self) -> IterationStatement {
        match self.peek() {
            Some(Token::While(..)) => {
                let w = read_token!(self, Token::While);
                let op = read_token!(self, Token::OpenParen);
                let e = Box::new(self.parse_expression().0);
                let cp = read_token!(self, Token::CloseParen);
                let s = Box::new(self.parse_statement(true));
                IterationStatement::While(w, op, e, cp, s)
            }
            Some(Token::Do(..)) => {
                let d = read_token!(self, Token::Do);
                let s = Box::new(self.parse_statement(true));
                let w = read_token!(self, Token::While);
                let op = read_token!(self, Token::OpenParen);
                let e = Box::new(self.parse_expression().0);
                let cp = read_token!(self, Token::CloseParen);
                let semi = read_token!(self, Token::Semicolon);
                IterationStatement::Do(d, s, w, op, e, cp, semi)
            }
            Some(Token::For(..)) => {
                let f = read_token!(self, Token::For);
                let op = read_token!(self, Token::OpenParen);
                let next = self.peek().cloned().unwrap();
                let e1 = if self.starts_type(next) {
                    DeclarationOrExpression::Declaration(Box::new(self.parse_declaration()))
                } else {
                    DeclarationOrExpression::ExpressionStatement(Box::new(
                        self.parse_expression_statement(true),
                    ))
                };
                let e2 = Box::new(self.parse_expression_statement(true));
                let f_expr = if let Some(Token::CloseParen(..)) = self.peek() {
                    ForExpr::EmptyLast(e1, e2)
                } else {
                    let e3 = Box::new(self.parse_expression().0);
                    ForExpr::ForExpr(e1, e2, e3)
                };
                let cp = read_token!(self, Token::CloseParen);
                let s = Box::new(self.parse_statement(true));
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
                    let e = self.parse_expression().0;
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
            _ => false,
        }
    }

    fn is_type(&self, ident: &str) -> bool {
        match ident {
            t if self.is_typeof(t) => true,
            "__label__" => true,
            _ => self.scope.iter().any(|e| e.types.contains(ident)),
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

    fn parse_expression(&mut self) -> (Expression, Option<i128>, Option<TypeSignature>) {
        let (first_expr, mut size, mut ty) = self.parse_assignment_expression();
        let mut exprs = vec![first_expr];
        while let Some(Token::Comma(..)) = self.peek() {
            read_token!(self, Token::Comma);
            let (e, s, t) = self.parse_assignment_expression();
            exprs.push(e);
            size = s;
            ty = t;
        }
        (Expression::Expression(exprs), size, ty)
    }

    fn parse_assignment_expression(
        &mut self,
    ) -> (AssignmentExpression, Option<i128>, Option<TypeSignature>) {
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
                )) = left.0
                {
                    let op = self.next();
                    let assignment_op = self.assignment_operator(op.unwrap().clone());
                    let right = self.parse_assignment_expression();
                    let res_val = left.1.as_ref().copied().and_then(|v1| {
                        right.1.as_ref().copied().map(|v2| match assignment_op {
                            AssignmentOperator::Assign(..) => v2,
                            AssignmentOperator::TimesAssign(..) => v1 * v2,
                            AssignmentOperator::DivAssign(..) => v1 / v2,
                            AssignmentOperator::ModAssign(..) => v1 % v2,
                            AssignmentOperator::PlusAssign(..) => v1 + v2,
                            AssignmentOperator::MinusAssign(..) => v1 - v2,
                            AssignmentOperator::BitShiftLeftAssign(..) => v1 << v2,
                            AssignmentOperator::BitShiftRightAssign(..) => v1 >> v2,
                            AssignmentOperator::BitAndAssign(..) => v1 & v2,
                            AssignmentOperator::BitXorAssign(..) => v1 ^ v2,
                            AssignmentOperator::BitOrAssign(..) => v1 | v2,
                        })
                    });
                    let res_ty = left.2.and_then(|v1| {
                        right.2.as_ref().cloned().map(|v2| match assignment_op {
                            AssignmentOperator::Assign(..) => v2,
                            _ => v1,
                        })
                    });
                    let res = AssignmentExpression::Assignment(
                        Box::new(u),
                        assignment_op,
                        Box::new(right.0),
                    );
                    (res, res_val, res_ty)
                } else {
                    panic!("Tried to assign to a rvalue")
                }
            }
            _ => (
                AssignmentExpression::TernaryExpression(left.0),
                left.1,
                left.2,
            ),
        }
    }

    fn parse_ternary_expression(
        &mut self,
    ) -> (TernaryExpression, Option<i128>, Option<TypeSignature>) {
        let e = self.parse_general_expression();
        if let Some(Token::Ternary(..)) = self.peek() {
            let t1 = read_token!(self, Token::Ternary);
            let if_true = self.parse_expression();
            let t2 = read_token!(self, Token::Colon);
            let otherwise = self.parse_ternary_expression();
            let expr = TernaryExpression::Ternary(
                *e.0,
                t1,
                Box::new(if_true.0),
                t2,
                Box::new(otherwise.0),
            );
            let expr_val = if let (Some(cond_val), Some(if_t_val), Some(if_f_val)) =
                (e.1, if_true.1, otherwise.1)
            {
                if cond_val != 0 {
                    Some(if_t_val)
                } else {
                    Some(if_f_val)
                }
            } else {
                None
            };
            let expr_ty = match (e.1, if_true.2, otherwise.2) {
                (Some(cond_val), Some(true_ty), Some(false_ty)) => {
                    if cond_val != 0 {
                        Some(true_ty)
                    } else {
                        Some(false_ty)
                    }
                }
                (_, t, _) => t,
            };
            (expr, expr_val, expr_ty)
        } else {
            (TernaryExpression::GeneralExpression(*e.0), e.1, e.2)
        }
    }

    fn parse_general_expression(
        &mut self,
    ) -> (Box<GeneralExpression>, Option<i128>, Option<TypeSignature>) {
        let left = self.parse_cast_expression();
        self.parse_general_expression_(
            1,
            Box::new(GeneralExpression::CastExpression(Box::new(left.0))),
            left.1,
            left.2,
        )
    }

    #[allow(clippy::cognitive_complexity)]
    #[rustfmt::skip]
    fn parse_general_expression_(
        &mut self,
        priority: usize,
        mut left: Box<GeneralExpression>,
        mut val: Option<i128>,
        mut ty: Option<TypeSignature>,
    ) -> (Box<GeneralExpression>, Option<i128>, Option<TypeSignature>) {
        loop {
            op_table!(
                left, val, ty, priority, self,
                (num, Times, 12, *)
                (num, Divide, 12, /)
                (num, Mod, 12, %)
                (num, Plus, 11, +)
                (num, Minus, 11, -)
                (num, BitShiftLeft, 10, <<)
                (num, BitShiftRight, 10, >>)
                (bool, LessThan, 9, <)
                (bool, MoreThan, 9, >)
                (bool, LessEqThan, 9, <=)
                (bool, MoreEqThan, 9, >=)
                (bool, Equals, 8, ==)
                (bool, NotEquals, 8, !=)
                (num, BitAnd, 7, &)
                (num, BitXor, 6, ^)
                (num, BitOr, 5, |)
                (bool, And, 4, &&)
                (bool, Or, 3, ||))
        }
        (left, val, ty)
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

    fn parse_cast_expression(&mut self) -> (CastExpression, Option<i128>, Option<TypeSignature>) {
        if let [Token::OpenParen(..), t] = self.peek_n(2).as_slice() {
            let tt = (**t).clone();
            if self.starts_type(&tt) && !self.starts_struct_literal() {
                let op = read_token!(self, Token::OpenParen);
                let ty = self.parse_type_name();
                let cp = read_token!(self, Token::CloseParen);
                let e = self.parse_general_expression();
                return (
                    CastExpression::OpenParen(op, Box::new(ty), cp, e.0),
                    e.1,
                    e.2,
                );
            }
        }
        let e = self.parse_unary_expression();
        (CastExpression::UnaryExpression(e.0), e.1, e.2)
    }

    fn parse_unary_expression(&mut self) -> (UnaryExpression, Option<i128>, Option<TypeSignature>) {
        match self.peek() {
            Some(Token::Increment(..)) | Some(Token::Decrement(..)) => {
                let t = self.next().cloned().unwrap();
                let op = self.increment_or_decrement(t);
                let e = self.parse_unary_expression();
                let val = e.1.map(|v| match op {
                    IncrementOrDecrement::Increment(..) => v + 1,
                    IncrementOrDecrement::Decrement(..) => v - 1,
                });
                (
                    UnaryExpression::IncrementOrDecrement(op, Box::new(e.0)),
                    val,
                    e.2,
                )
            }
            Some(Token::BitAnd(..))
            | Some(Token::Times(..))
            | Some(Token::Plus(..))
            | Some(Token::Minus(..))
            | Some(Token::BitNot(..))
            | Some(Token::Not(..)) => {
                let t = self.next().cloned().unwrap();
                let op = self.unary_operator(t);
                let e = self.parse_cast_expression();
                let val = e.1.and_then(|v| match op {
                    UnaryOperator::BitAnd(..) | UnaryOperator::Times(..) => None,
                    UnaryOperator::Plus(..) => Some(v),
                    UnaryOperator::Minus(..) => Some(-v),
                    UnaryOperator::BitNot(..) => Some(!v),
                    UnaryOperator::Not(..) => Some(if v != 0 { 0 } else { 1 }),
                });
                let ty = e.2.map(|t| match op {
                    UnaryOperator::BitAnd(..) => TypeSignature::Pointer {
                        ty: Box::new(t),
                        nullable: false,
                    },
                    UnaryOperator::Times(..) => t.dereference(&HashMap::new()).unwrap(),
                    UnaryOperator::Plus(..)
                    | UnaryOperator::Minus(..)
                    | UnaryOperator::BitNot(..)
                    | UnaryOperator::Not(..) => t,
                });
                (UnaryExpression::UnaryOperator(op, Box::new(e.0)), val, ty)
            }
            Some(Token::Sizeof(..)) => {
                read_token!(self, Token::Sizeof);
                if let [Token::OpenParen(..), t] = self.peek_n(2).as_slice() {
                    let tt = (**t).clone();
                    if self.starts_type(&tt) {
                        read_token!(self, Token::OpenParen);
                        let ty = self.parse_type_name();
                        read_token!(self, Token::CloseParen);
                        return (
                            UnaryExpression::SizeofTy(Box::new(ty)),
                            None,
                            Some(TypeSignature::Primitive(Primitive::UInt(64))),
                        );
                    }
                }
                let e = self.parse_unary_expression();
                (
                    UnaryExpression::SizeofExpr(Box::new(e.0)),
                    None,
                    Some(TypeSignature::Primitive(Primitive::UInt(64))),
                )
            }
            /* GNU extension */
            Some(Token::And(..)) => {
                read_token!(self, Token::And);
                let ident = read_token!(self, Token::Identifier).get_ident_str().clone();
                (UnaryExpression::AddressOfLabel(ident), None, None)
            }
            _ => {
                let e = self.parse_postfix_expression();
                (UnaryExpression::PostfixExpression(Box::new(e.0)), e.1, e.2)
            }
        }
    }

    fn parse_postfix_expression(
        &mut self,
    ) -> (PostfixExpression, Option<i128>, Option<TypeSignature>) {
        let expr = self.parse_primary_expression();
        let mut e = PostfixExpression::PrimaryExpression(expr.0);
        let mut val = expr.1;
        let mut ty = expr.2;
        loop {
            match self.peek() {
                Some(Token::OpenBracket(..)) => {
                    let op = read_token!(self, Token::OpenBracket);
                    let expr = self.parse_expression();
                    let cp = read_token!(self, Token::CloseBracket);
                    val = None;
                    ty = ty.map(|t| t.dereference(&HashMap::new()).unwrap());
                    e = PostfixExpression::Index(Box::new(e), op, Box::new(expr.0), cp);
                }
                Some(Token::Dot(..)) => {
                    let dot = read_token!(self, Token::Dot);
                    let ident = read_token!(self, Token::Identifier);
                    val = None;
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
                            list.push(self.parse_assignment_expression().0);
                            if let Some(Token::Comma(..)) = self.peek() {
                                read_token!(self, Token::Comma);
                            } else {
                                break;
                            }
                        }
                    }
                    let cp = read_token!(self, Token::CloseParen);
                    e = PostfixExpression::Call(
                        Box::new(e),
                        op,
                        ArgumentExpressionList::List(list),
                        cp,
                    );
                }
                Some(Token::Increment(..)) | Some(Token::Decrement(..)) => {
                    let t = self.next().cloned().unwrap();
                    let inc = self.increment_or_decrement(t);
                    e = PostfixExpression::Increment(Box::new(e), inc);
                }
                _ => return (e, val, ty),
            }
        }
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_primary_expression(
        &mut self,
    ) -> (PrimaryExpression, Option<i128>, Option<TypeSignature>) {
        match self.peek() {
            Some(Token::Identifier(..)) => {
                let s = read_identifier_str!(self);
                let val = self.get_val(&s);
                if self.is_builtin(&s) {
                    (self.parse_builtin(s), None, None)
                } else {
                    (
                        PrimaryExpression::Identifier(s),
                        val,
                        val.map(|_| TypeSignature::int()),
                    )
                }
            }
            Some(Token::Number(..)) => {
                let t = self.next().unwrap().get_ident_str().clone();
                let mut lc = t.to_ascii_lowercase();
                let mut float = None;
                while lc.ends_with('l')
                    || lc.ends_with('u')
                    || lc.ends_with('f')
                    || lc.ends_with('d')
                {
                    if lc.ends_with('f') {
                        float = Some(TypeSignature::Primitive(Primitive::Float));
                    } else if lc.ends_with('d') {
                        float = Some(TypeSignature::Primitive(Primitive::Double));
                    }
                    lc.pop();
                }
                if lc.contains('.') {
                    float = Some(TypeSignature::Primitive(Primitive::Float));
                }
                if let Some(ty) = float {
                    (PrimaryExpression::Number(t), None, Some(ty))
                } else {
                    let val = utils::int_from_str(&lc);
                    (
                        PrimaryExpression::Number(t),
                        Some(val),
                        Some(TypeSignature::int()),
                    )
                }
            }
            Some(Token::StringLiteral(..)) => (
                PrimaryExpression::StringLiteral(self.next().unwrap().get_ident_str().clone()),
                None,
                Some(TypeSignature::char().address_of()),
            ),
            Some(Token::CharLiteral(..)) => {
                let val = self.next().unwrap().get_char_val();
                (
                    PrimaryExpression::CharLiteral(val),
                    Some(val as i128),
                    Some(TypeSignature::char()),
                )
            }
            Some(Token::OpenParen(..)) => {
                read_token!(self, Token::OpenParen);
                if let Some(Token::OpenBrace(..)) = self.peek() {
                    let compound = self.parse_compound_statement();
                    read_token!(self, Token::CloseParen);
                    (PrimaryExpression::Statement(Box::new(compound)), None, None)
                } else if self.peek().cloned().map(|t| self.starts_type(t)) == Some(true) {
                    let ty = self.parse_type_name();
                    read_token!(self, Token::CloseParen);
                    read_token!(self, Token::OpenBrace);
                    let initializers = self.parse_initializer_list();
                    read_token!(self, Token::CloseBrace);
                    (
                        PrimaryExpression::StructValue(Box::new(ty), initializers),
                        None,
                        None,
                    )
                } else {
                    let e = self.parse_expression();
                    read_token!(self, Token::CloseParen);
                    (PrimaryExpression::Expression(Box::new(e.0)), e.1, e.2)
                }
            }
            t => unexpected_token!(t, self),
        }
    }

    fn is_builtin(&self, name: &Rc<str>) -> bool {
        match name.as_ref() {
            "__builtin_offsetof" => true,
            "__builtin_types_compatible_p" => true,
            _ => false,
        }
    }

    fn parse_builtin(&mut self, name: Rc<str>) -> PrimaryExpression {
        let extension = match name.as_ref() {
            "__builtin_offsetof" => {
                read_token!(self, Token::OpenParen);
                let ty = Box::new(self.parse_type_name());
                read_token!(self, Token::Comma);
                let mut designator = Box::new(BuiltinDesignator::Identifier(
                    read_token!(self, Token::Identifier).get_ident_str().clone(),
                ));

                loop {
                    match self.peek() {
                        Some(Token::Dot(..)) => {
                            read_token!(self, Token::Dot);
                            designator = Box::new(BuiltinDesignator::Field(
                                designator,
                                read_token!(self, Token::Identifier).get_ident_str().clone(),
                            ));
                        }
                        Some(Token::OpenBracket(..)) => {
                            read_token!(self, Token::OpenBracket);
                            designator = Box::new(BuiltinDesignator::Index(
                                designator,
                                Box::new(self.parse_expression().0),
                            ));
                            read_token!(self, Token::CloseBracket);
                        }
                        _ => break,
                    }
                }

                read_token!(self, Token::CloseParen);

                Builtin::Offsetof(ty, designator)
            }
            "__builtin_types_compatible_p" => {
                read_token!(self, Token::OpenParen);
                let left = Box::new(self.parse_type_name());
                read_token!(self, Token::Comma);
                let right = Box::new(self.parse_type_name());
                read_token!(self, Token::CloseParen);
                Builtin::TypesCompatible(left, right)
            }
            _ => panic!(),
        };

        PrimaryExpression::Builtin(Box::new(extension))
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
        if let ExternalDeclaration::Declaration(box Declaration::Declaration(_, list, _)) = ext_decl
        {
            list.iter()
                .map(|e| match e {
                    InitDeclarator::Declarator(decl) => self.get_decl_identifier(&**decl),
                    _ => panic!(),
                })
                .collect()
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
            DirectDeclarator::AsmStatement(_) => panic!(),
            DirectDeclarator::Parens(decl) => self.get_decl_identifier(decl),
            DirectDeclarator::Array(direct_decl, _) => self.get_direct_decl_identifier(direct_decl),
            DirectDeclarator::Function(direct_decl, _) => {
                self.get_direct_decl_identifier(direct_decl)
            }
        }
    }

    fn get_type(&self, _expr: Expression) -> TypeName {
        unimplemented!();
    }

    fn maybe_parse_attributes(&mut self) -> Vec<Attribute> {
        let mut attrs = Vec::new();
        loop {
            match self.peek() {
                Some(Token::Identifier(_, ident)) if ident.as_ref() == "__attribute__" => {
                    if_debug(DebugVal::CParserToken, || {
                        println!("Parsing an __attribute__");
                    });
                    read_token!(self, Token::Identifier);
                    read_token!(self, Token::OpenParen);
                    read_token!(self, Token::OpenParen);
                    loop {
                        match self.peek() {
                            Some(Token::Comma(..)) => {
                                read_token!(self, Token::Comma);
                            }
                            Some(Token::Identifier(_, ident))
                                if ident.as_ref() == "vector_size"
                                    || ident.as_ref() == "__vector_size__" =>
                            {
                                read_token!(self, Token::Identifier);
                                read_token!(self, Token::OpenParen);
                                let size = read_token!(self, Token::Number)
                                    .get_ident_str()
                                    .parse()
                                    .unwrap();
                                attrs.push(Attribute::Vector(size));
                                read_token!(self, Token::CloseParen);
                            }
                            Some(Token::Identifier(..)) => {
                                self.parse_attribute();
                                if maybe_read_token!(self, Token::Comma).is_some() {
                                    continue;
                                } else {
                                    break;
                                }
                            }
                            _ => break,
                        }
                    }
                    read_token!(self, Token::CloseParen);
                    read_token!(self, Token::CloseParen);
                }
                _ => {
                    break;
                }
            }
        }
        attrs
    }

    fn parse_attribute(&mut self) {
        read_token!(self, Token::Identifier);
        if let Some(Token::OpenParen(..)) = self.peek() {
            read_token!(self, Token::OpenParen);
            loop {
                if maybe_read_token!(self, Token::CloseParen).is_some() {
                    break;
                } else {
                    self.parse_assignment_expression();
                    if maybe_read_token!(self, Token::Comma).is_some() {
                        continue;
                    } else {
                        read_token!(self, Token::CloseParen);
                        break;
                    }
                }
            }
        }
    }

    #[allow(unreachable_patterns)]
    fn parse_type_statement_or_expression(&mut self) -> Vec<MacroExpansion> {
        let mut res = Vec::new();
        loop {
            let m = match self.peek() {
                None => break,
                Some(Token::Semicolon(..)) => {
                    read_token!(self, Token::Semicolon);
                    continue;
                }
                Some(Token::Identifier(_, ident)) => {
                    if !self.is_type(&ident) && !self.is_typeof(ident) {
                        let m = match self.parse_statement(false) {
                            Statement::ExpressionStatement(
                                box ExpressionStatement::Expression(Some(e))) => MacroExpansion::Expression(*e),
                                Statement::ExpressionStatement(
                                    box ExpressionStatement::Expression(None)) => continue,
                                stmt => MacroExpansion::Statement(stmt),
                        };
                        res.push(m);
                        continue;
                    }
                    MacroExpansion::Declaration(self.parse_external_declaration())
                },
                Some(t) => {
                    let tt = t.clone();
                    if self.starts_type(&tt) {
                        MacroExpansion::Declaration(self.parse_external_declaration())
                    } else {
                        match self.parse_statement(false) {
                            Statement::ExpressionStatement(box ExpressionStatement::Expression(Some(e))) => MacroExpansion::Expression(*e),
                            Statement::ExpressionStatement(box ExpressionStatement::Expression(None)) => continue,
                            stmt => MacroExpansion::Statement(stmt),
                        }
                    }
                }
            };
            res.push(m);
        }
        res
    }
}

pub fn get_declarations(tree: &S) -> (Declarations, Declarations) {
    DeclarationContext {
        types: HashMap::new(),
    }
    .get_declarations(tree)
}

struct DeclarationContext {
    types: HashMap<Rc<str>, TypeSignature>,
}

impl DeclarationContext {
    pub fn get_declarations(mut self, tree: &S) -> (Declarations, Declarations) {
        let mut declarations = Vec::new();
        let mut types = Vec::new();
        let S::TranslationUnit(TranslationUnit::Units(units)) = tree;

        for unit in units {
            let (mut decls, ty) = match &unit {
                ExternalDeclaration::FunctionDefinition(def) => {
                    self.function_definition_to_type(def)
                }
                ExternalDeclaration::Declaration(def) => {
                    let (mut decls, has_ty_defs, ty) = self.declaration_to_type(def);
                    if has_ty_defs {
                        for (name, ty) in decls.into_iter() {
                            types.push((name.clone(), ty.clone()));
                        }
                        decls = Vec::new();
                    }
                    (decls, ty)
                }
                ExternalDeclaration::Semicolon(_) => continue,
            };
            match &ty {
                TypeSignature::Struct(Some(name), _)
                | TypeSignature::Enum(Some(name), _)
                | TypeSignature::Union(Some(name), _) => {
                    self.types.insert(name.clone(), ty.clone());
                    types.push((name.clone(), ty.clone()));
                }
                _ => {}
            }
            declarations.append(&mut decls);
        }

        (declarations, types)
    }

    fn function_definition_to_type(
        &self,
        decl: &FunctionDefinition,
    ) -> (Declarations, TypeSignature) {
        let FunctionDefinition::FunctionDefinition(spec, decl, _) = decl;
        let spec_ty = self.decl_spec_to_type(spec.as_ref().unwrap(), Vec::new());
        (
            vec![self.declarator_to_type(decl, spec_ty.clone())],
            spec_ty,
        )
    }

    fn declaration_to_type(&self, decl: &Declaration) -> (Declarations, bool, TypeSignature) {
        match decl {
            Declaration::Declaration(spec, init_decls, attrs) => {
                let spec_ty =
                    self.decl_spec_to_type(&**spec, attrs.clone().unwrap_or_else(Vec::new));
                let decl_tys = init_decls
                    .iter()
                    .map(|init_decl| match init_decl {
                        InitDeclarator::Declarator(decl)
                        | InitDeclarator::Asm(decl, _)
                        | InitDeclarator::Assign(decl, _, _) => {
                            self.declarator_to_type(&**decl, spec_ty.clone())
                        }
                    })
                    .collect();
                (decl_tys, has_typedef(&**spec), spec_ty)
            }
            Declaration::Pragma(..) => unreachable!(),
        }
    }

    fn decl_spec_to_type(
        &self,
        spec: &DeclarationSpecifiers,
        attrs: Vec<Attribute>,
    ) -> TypeSignature {
        let DeclarationSpecifiers::DeclarationSpecifiers(_, ty) = spec;
        ty.as_ref()
            .map(|t| self.type_specifier_to_type(t.clone(), attrs))
            .unwrap()
    }

    fn declarator_to_type(&self, decl: &Declarator, ty: TypeSignature) -> (Rc<str>, TypeSignature) {
        let Declarator::Declarator(ptr, decl) = decl;
        let ty = if let Some(p) = ptr {
            self.ptr_to_ty(p, Box::new(ty))
        } else {
            ty
        };
        self.direct_decl_to_type(decl, ty)
    }

    fn abstract_declarator_to_type(
        &self,
        decl: &AbstractDeclarator,
        ty: TypeSignature,
    ) -> TypeSignature {
        let AbstractDeclarator::AbstractDeclarator(ptr, decl) = decl;
        let ty = if let Some(p) = ptr {
            self.ptr_to_ty(p, Box::new(ty))
        } else {
            ty
        };
        self.direct_abstract_decl_to_type(&**decl, ty)
    }

    fn direct_decl_to_type(
        &self,
        decl: &DirectDeclarator,
        ty: TypeSignature,
    ) -> (Rc<str>, TypeSignature) {
        match decl {
            DirectDeclarator::Identifier(ident) => (ident.clone(), ty),
            DirectDeclarator::AsmStatement(_) => (From::from("_"), ty),
            DirectDeclarator::Parens(decl) => self.declarator_to_type(&**decl, ty),
            DirectDeclarator::Array(decl, None) => {
                let (name, ty) = self.direct_decl_to_type(&**decl, ty);
                (name, TypeSignature::Array(Box::new(ty), None))
            }
            DirectDeclarator::Array(decl, Some(_e)) => {
                let (name, ty) = self.direct_decl_to_type(&**decl, ty);
                (name, TypeSignature::Array(Box::new(ty), Some(0)))
            }
            DirectDeclarator::Function(decl, params) => {
                let (name, ty) = self.direct_decl_to_type(&**decl, ty);
                (
                    name,
                    TypeSignature::Function(
                        params.iter().map(|f| self.function_param(f)).collect(),
                        Box::new(ty),
                    ),
                )
            }
        }
    }

    fn direct_abstract_decl_to_type(
        &self,
        decl: &DirectAbstractDeclarator,
        ty: TypeSignature,
    ) -> TypeSignature {
        match decl {
            DirectAbstractDeclarator::Epsilon() => ty,
            DirectAbstractDeclarator::Parens(decl) => self.abstract_declarator_to_type(&**decl, ty),
            DirectAbstractDeclarator::Array(decl, None) => {
                let ty = self.direct_abstract_decl_to_type(&**decl, ty);
                TypeSignature::Array(Box::new(ty), None)
            }
            DirectAbstractDeclarator::Array(decl, Some(_e)) => {
                let ty = self.direct_abstract_decl_to_type(&**decl, ty);
                TypeSignature::Array(Box::new(ty), Some(0))
            }
            DirectAbstractDeclarator::Function(decl, params) => {
                let ty = self.direct_abstract_decl_to_type(&**decl, ty);
                TypeSignature::Function(
                    params.iter().map(|f| self.function_param(f)).collect(),
                    Box::new(ty),
                )
            }
        }
    }

    fn ptr_to_ty(&self, ptr: &Pointer, ty: Box<TypeSignature>) -> TypeSignature {
        let Pointer::Ptr(_, p) = ptr;
        if let Some(p) = p {
            self.ptr_to_ty(p, Box::new(TypeSignature::Pointer { ty, nullable: true }))
        } else {
            TypeSignature::Pointer { ty, nullable: true }
        }
    }

    fn type_specifier_to_type(
        &self,
        mut ty: TypeSpecifier,
        attrs: Vec<Attribute>,
    ) -> TypeSignature {
        use CType::Primitive as CP;
        use PrimitiveType::*;
        use TypeSignature::Primitive as P;

        let mut complex = false;

        if let CType::Complex(sign, t) = ty {
            ty = CType::Primitive(sign, t);
            complex = true;
        }

        let ty = match ty {
            CType::Void => P(Primitive::Void),
            CP(None, Char) => P(Primitive::Char),
            CP(Some(true), Char) => P(Primitive::Int(8)),
            CP(Some(false), Char) => P(Primitive::UInt(8)),

            CP(None, Short) | CP(Some(true), Short) => P(Primitive::Int(16)),
            CP(Some(false), Short) => P(Primitive::UInt(16)),

            CP(None, Int) | CP(Some(true), Int) => P(Primitive::Int(32)),
            CP(Some(false), Int) => P(Primitive::UInt(32)),

            CP(None, Long)
            | CP(Some(true), Long)
            | CP(None, LongLong)
            | CP(Some(true), LongLong) => P(Primitive::Int(64)),
            CP(Some(false), Long) | CP(Some(false), LongLong) => P(Primitive::UInt(64)),

            CP(None, Float) | CP(Some(true), Float) => P(Primitive::Float),
            CP(Some(false), Float) => P(Primitive::Float),

            CP(None, Double) | CP(Some(true), Double) => P(Primitive::Double),
            CP(Some(false), Double) => P(Primitive::Double),

            CP(None, LongDouble) | CP(Some(true), LongDouble) => P(Primitive::Double),
            CP(Some(false), LongDouble) => P(Primitive::Double),

            CType::Compound(compound) => self.compound_type_to_type(&compound),
            CType::Custom(ty) => TypeSignature::Plain(ty.clone()),
            CType::Complex(..) => unreachable!(),
        };

        if let P(prim) = ty {
            if complex {
                return TypeSignature::Complex(prim);
            }
        }

        if let P(prim) = ty {
            if !attrs.is_empty() {
                return TypeSignature::Vector(prim);
            }
        }

        ty
    }

    fn compound_type_to_type(&self, ty: &CompoundType) -> TypeSignature {
        match ty {
            CompoundType::Struct(name, Some(fields)) => TypeSignature::Struct(
                Some(name.clone()),
                fields
                    .iter()
                    .flat_map(|f| self.struct_field(f).into_iter())
                    .collect(),
            ),
            CompoundType::Struct(name, None) => self
                .types
                .get(name)
                .cloned()
                .unwrap_or_else(|| TypeSignature::Plain(name.clone())),
            CompoundType::AnonymousStruct(fields) => TypeSignature::Struct(
                None,
                fields
                    .iter()
                    .flat_map(|f| self.struct_field(f).into_iter())
                    .collect(),
            ),
            CompoundType::Union(name, Some(fields)) => TypeSignature::Union(
                Some(name.clone()),
                fields
                    .iter()
                    .flat_map(|f| self.struct_field(f).into_iter())
                    .collect(),
            ),
            CompoundType::Union(name, None) => self.types[name].clone(),
            CompoundType::AnonymousUnion(fields) => TypeSignature::Union(
                None,
                fields
                    .iter()
                    .flat_map(|f| self.struct_field(f).into_iter())
                    .collect(),
            ),
            CompoundType::Enum(name, Some(fields)) => TypeSignature::Enum(
                Some(name.clone()),
                fields.iter().map(|f| self.enum_field(f)).collect(),
            ),
            CompoundType::Enum(name, None) => self.types[name].clone(),
            CompoundType::AnonymousEnum(fields) => {
                TypeSignature::Enum(None, fields.iter().map(|f| self.enum_field(f)).collect())
            }
        }
    }

    fn struct_field(&self, f: &StructField) -> Vec<purkkasyntax::StructField> {
        let spec_ty = self.decl_spec_to_type(&*f.0, Vec::new());
        f.1.iter()
            .map(|(decl, maybe_bitfield)| {
                let bitfield = maybe_bitfield
                    .as_ref()
                    .map(|b| usize::try_from(b.value().unwrap()).unwrap());

                let (name, ty) = match decl {
                    EitherDeclarator::Anonymous(_) => unimplemented!(),
                    EitherDeclarator::Declarator(decl) => {
                        self.declarator_to_type(decl, spec_ty.clone())
                    }
                };
                purkkasyntax::StructField::Field {
                    name,
                    ty: Box::new(ty),
                    bitfield,
                }
            })
            .collect()
    }

    fn enum_field(&self, f: &EnumField) -> purkkasyntax::EnumField {
        purkkasyntax::EnumField::Field {
            name: f.0.clone(),
            value: f.2,
            ty: None,
        }
    }

    fn function_param(&self, f: &FunctionParam) -> purkkasyntax::Param {
        match f {
            FunctionParam::Identifier(_name) => unimplemented!(),
            FunctionParam::Parameter(param) => match param {
                ParameterDeclaration::Declarator(spec, decl) => {
                    let spec_ty = self.decl_spec_to_type(spec, Vec::new());
                    let (name, ty) = self.declarator_to_type(decl, spec_ty);
                    purkkasyntax::Param::Param(name, Box::new(ty))
                }
                ParameterDeclaration::AbstractDeclarator(spec, decl) => {
                    let spec_ty = self.decl_spec_to_type(spec, Vec::new());
                    let ty = self.abstract_declarator_to_type(decl, spec_ty);
                    purkkasyntax::Param::Param(From::from("_"), Box::new(ty))
                }
                ParameterDeclaration::DeclarationSpecifiers(spec) => {
                    let spec_ty = self.decl_spec_to_type(spec, Vec::new());
                    purkkasyntax::Param::Param(From::from("_"), Box::new(spec_ty))
                }
            },
            FunctionParam::Varargs => purkkasyntax::Param::Variadic,
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
