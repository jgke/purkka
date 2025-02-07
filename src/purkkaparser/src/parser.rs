use std::collections::{HashMap, HashSet, VecDeque};
use std::convert::TryFrom;
use std::rc::Rc;

use regex::Regex;

use fragment::fragment::{FragmentIterator, Source};
use purkkasyntax::*;
use purkkatoken::token::Token;
use resolve::{FileQuery, ResolveResult};

use crate::builtins::insert_builtins;

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

#[derive(Debug, Clone)]
pub struct Operator {
    pub precedence: usize,
    pub param_count: usize,
    pub left_associative: bool,

    pub ty: TypeSignature,
    pub handler: Option<Expression>,
}

pub type OperatorMap = HashMap<Rc<str>, Operator>;
pub type Types = (HashMap<Rc<str>, TypeSignature>, Vec<Rc<str>>);

#[derive(Debug, Clone, Default)]
pub struct Operators {
    pub unary: OperatorMap,
    pub infix: OperatorMap,
    pub postfix: OperatorMap,
}

#[derive(Debug, Default, Clone)]
pub struct Symbols {
    pub types: (HashMap<Rc<str>, (TypeSignature, Option<Rc<str>>)>, Vec<Rc<str>>),
    pub declarations: Types,
    pub imported_types: Types,
    pub imported_declarations: Types,
    pub imported_macros: (HashSet<Rc<str>>, HashSet<Rc<str>>),
}

fn unary_num_to_bool() -> TypeSignature {
    let intermediate = IntermediateType::generic_number(IntermediateNumber::Indeterminate);
    let num = Box::new(TypeSignature::Infer(intermediate));
    TypeSignature::Function(
        vec![Param::TypeOnly(num.clone())],
        Box::new(TypeSignature::Primitive(Primitive::Int(32))),
    )
}

fn unary_num_to_num() -> TypeSignature {
    let intermediate = IntermediateType::generic_number(IntermediateNumber::Indeterminate);
    let num = Box::new(TypeSignature::Infer(intermediate));
    TypeSignature::Function(vec![Param::TypeOnly(num.clone())], num.clone())
}

fn unary_any_to_ptr() -> TypeSignature {
    let intermediate = IntermediateType::generic_any();
    let num = Box::new(TypeSignature::Infer(intermediate));
    TypeSignature::Function(
        vec![Param::TypeOnly(num.clone())],
        Box::new(TypeSignature::Pointer {
            nullable: false,
            ty: num.clone(),
        }),
    )
}

fn bin_num_to_bool() -> TypeSignature {
    let intermediate = IntermediateType::generic_number(IntermediateNumber::Indeterminate);
    let num = Box::new(TypeSignature::Infer(intermediate));
    TypeSignature::Function(
        vec![Param::TypeOnly(num.clone()), Param::TypeOnly(num.clone())],
        Box::new(TypeSignature::Primitive(Primitive::Int(32))),
    )
}

fn bin_num_to_num() -> TypeSignature {
    let intermediate = IntermediateType::generic_number(IntermediateNumber::Indeterminate);
    let num = Box::new(TypeSignature::Infer(intermediate));
    TypeSignature::Function(
        vec![Param::TypeOnly(num.clone()), Param::TypeOnly(num.clone())],
        num.clone(),
    )
}

fn bin_any_to_any() -> TypeSignature {
    let intermediate = IntermediateType::generic_any();
    let num = Box::new(TypeSignature::Infer(intermediate));
    TypeSignature::Function(
        vec![Param::TypeOnly(num.clone()), Param::TypeOnly(num.clone())],
        num.clone(),
    )
}

#[rustfmt::skip]
fn default_bin_ops() -> OperatorMap {
    let mut infix_operators = HashMap::new();

    // Assignment
    infix_operators.insert(From::from("="), Operator::binop_right(1, bin_any_to_any(), None));
    infix_operators.insert(From::from("*="), Operator::binop_right(1, bin_any_to_any(), None)); 
    infix_operators.insert(From::from("/="), Operator::binop_right(1, bin_any_to_any(), None));
    infix_operators.insert(From::from("%="), Operator::binop_right(1, bin_any_to_any(), None));
    infix_operators.insert(From::from("+="), Operator::binop_right(1, bin_any_to_any(), None));
    infix_operators.insert(From::from("-="), Operator::binop_right(1, bin_any_to_any(), None));
    infix_operators.insert(From::from("<<="), Operator::binop_right(1, bin_any_to_any(), None));
    infix_operators.insert(From::from(">>="), Operator::binop_right(1, bin_any_to_any(), None));
    infix_operators.insert(From::from("&="), Operator::binop_right(1, bin_any_to_any(), None));
    infix_operators.insert(From::from("^="), Operator::binop_right(1, bin_any_to_any(), None));
    infix_operators.insert(From::from("|="), Operator::binop_right(1, bin_any_to_any(), None));

    // Ternary
    infix_operators.insert(From::from("?"), Operator::binop_right(2, bin_num_to_num(), None));

    // Logical operations: or, and, eq, neq, leq, meq, less, more
    infix_operators.insert(From::from("||"), Operator::binop(3, bin_num_to_bool(), None));
    infix_operators.insert(From::from("&&"), Operator::binop(4, bin_num_to_bool(), None));
    infix_operators.insert(From::from("=="), Operator::binop(5, bin_num_to_bool(), None));
    infix_operators.insert(From::from("!="), Operator::binop(5, bin_num_to_bool(), None));
    infix_operators.insert(From::from("<="), Operator::binop(6, bin_num_to_bool(), None));
    infix_operators.insert(From::from(">="), Operator::binop(6, bin_num_to_bool(), None));
    infix_operators.insert(From::from("<"), Operator::binop(6, bin_num_to_bool(), None));
    infix_operators.insert(From::from(">"), Operator::binop(6, bin_num_to_bool(), None));

    // Bitwise operations: or, xor, and, shl, shr, rotating bitshifts
    infix_operators.insert(From::from("|"), Operator::binop(7, bin_num_to_num(), None));
    infix_operators.insert(From::from("^"), Operator::binop(8, bin_num_to_num(), None));
    infix_operators.insert(From::from("&"), Operator::binop(9, bin_num_to_num(), None));
    infix_operators.insert(From::from("<<"), Operator::binop(10, bin_num_to_num(), None));
    infix_operators.insert(From::from(">>"), Operator::binop(10, bin_num_to_num(), None));
    infix_operators.insert(From::from("<<<"), Operator::binop(10, bin_num_to_num(), None));
    infix_operators.insert(From::from(">>>"), Operator::binop(10, bin_num_to_num(), None));

    // Standard arithmetic: plus, minus, mod, times, div, pow
    infix_operators.insert(From::from("+"), Operator::binop(11, bin_num_to_num(), None));
    infix_operators.insert(From::from("-"), Operator::binop(11, bin_num_to_num(), None));
    infix_operators.insert(From::from("%"), Operator::binop(12, bin_num_to_num(), None));
    infix_operators.insert(From::from("*"), Operator::binop(12, bin_num_to_num(), None));
    infix_operators.insert(From::from("/"), Operator::binop(12, bin_num_to_num(), None));

    infix_operators
}

#[rustfmt::skip]
fn default_unary_ops() -> OperatorMap {
    let mut unary_operators = HashMap::new();

    // Unary plus (nop), unary minus (negation), logical not (!= 0), bitwise not,
    // addressof, dereference, prefix increment/decrement
    unary_operators.insert(From::from("+"), Operator::unary(unary_num_to_num(), None));
    unary_operators.insert(From::from("-"), Operator::unary(unary_num_to_num(), None));
    unary_operators.insert(From::from("!"), Operator::unary(unary_num_to_bool(), None));
    unary_operators.insert(From::from("~"), Operator::unary(unary_num_to_num(), None));
    unary_operators.insert(From::from("&"), Operator::unary(unary_any_to_ptr(), None));
    unary_operators.insert(From::from("*"), Operator::unary(unary_num_to_num(), None));
    unary_operators.insert(From::from("++"), Operator::unary(unary_num_to_num(), None));
    unary_operators.insert(From::from("--"), Operator::unary(unary_num_to_num(), None));

    unary_operators
}

#[rustfmt::skip]
fn default_postfix_ops() -> OperatorMap {
    let mut postfix_operators = HashMap::new();

    // Postfix increment, decrement
    postfix_operators.insert(From::from("++"), Operator::unary_right(unary_num_to_num(), None));
    postfix_operators.insert(From::from("--"), Operator::unary_right(unary_num_to_num(), None));

    postfix_operators
}

fn default_symbols() -> Symbols {
    let mut imported_declarations = Types::default();
    insert_builtins(&mut imported_declarations);
    Symbols {
        types: (HashMap::new(), Vec::new()),
        declarations: Types::default(),
        imported_types: Types::default(),
        imported_declarations,
        imported_macros: (HashSet::new(), HashSet::new()),
    }
}

pub fn parse(
    iter: Vec<Token>,
    sources: &[Source],
    fragment_iter: &FragmentIterator,
    current_file: &str,
    get_file: &dyn Fn(FileQuery) -> ResolveResult,
    expand: &dyn Fn(String, HashSet<Rc<str>>) -> Vec<MacroExpansion>,
    expand_call: &dyn Fn(String, Vec<Expression>, HashSet<Rc<str>>) -> Vec<MacroExpansion>,
) -> (S, Operators, Symbols) {
    let symbols = default_symbols();
    let mut context = ParseContext {
        operators: Operators {
            unary: default_unary_ops(),
            postfix: default_postfix_ops(),
            infix: default_bin_ops(),
        },
        iter: From::from(iter),
        fragment: fragment_iter,
        sources,
        symbols,
        current_identifiers: vec![HashSet::new()],
        current_file,
        get_file,
        expand,
        expand_call,
        define_buf: String::new(),
    };
    let tu = S::TranslationUnit(context.parse_translation_unit());
    (tu, context.operators, context.symbols)
}

impl Operator {
    fn binop(precedence: usize, ty: TypeSignature, handler: Option<Expression>) -> Operator {
        Operator {
            precedence,
            param_count: 2,
            left_associative: true,
            ty,
            handler,
        }
    }
    fn binop_right(precedence: usize, ty: TypeSignature, handler: Option<Expression>) -> Operator {
        Operator {
            precedence,
            param_count: 2,
            left_associative: false,
            ty,
            handler,
        }
    }
    fn unary(ty: TypeSignature, handler: Option<Expression>) -> Operator {
        Operator {
            precedence: 15,
            param_count: 1,
            left_associative: true,
            ty,
            handler,
        }
    }
    fn unary_right(ty: TypeSignature, handler: Option<Expression>) -> Operator {
        Operator {
            precedence: 15,
            param_count: 1,
            left_associative: false,
            ty,
            handler,
        }
    }
}

struct ParseContext<'a> {
    operators: Operators,
    symbols: Symbols,
    current_identifiers: Vec<HashSet<Rc<str>>>,

    iter: VecDeque<Token>,
    fragment: &'a FragmentIterator,
    sources: &'a [Source],

    current_file: &'a str,
    get_file: &'a dyn Fn(FileQuery) -> ResolveResult,
    expand: &'a dyn Fn(String, HashSet<Rc<str>>) -> Vec<MacroExpansion>,
    expand_call: &'a dyn Fn(String, Vec<Expression>, HashSet<Rc<str>>) -> Vec<MacroExpansion>,

    define_buf: String
}

impl<'a> ParseContext<'a> {
    fn next(&mut self) -> Option<Token> {
        self.iter.pop_front()
    }
    fn peek(&self) -> Option<Token> {
        self.iter.get(0).cloned()
    }
    fn parse_translation_unit(&mut self) -> TranslationUnit {
        let mut units = Vec::new();
        while maybe_read_token!(self, Token::SemiColon).is_some() {}
        while self.peek().is_some() {
            units.append(&mut self.parse_unit());
            while maybe_read_token!(self, Token::SemiColon).is_some() {}
        }
        TranslationUnit::Units(units)
    }

    fn get_ty(&self, s: &str) -> Option<&TypeSignature> {
        self.symbols
            .types.0
            .get(s).as_ref().map(|t| &t.0)
            .or_else(|| self.symbols.imported_types.0.get(s))
    }

    fn push_operator(
        &mut self,
        left_associative: bool,
        precedence: usize,
        s: Rc<str>,
        ty: TypeSignature,
        body: Expression,
    ) {
        if self.operators.infix.contains_key(&s) {
            panic!("Operator {} already defined", s);
        }

        if left_associative {
            self.operators
                .infix
                .insert(s, Operator::binop(precedence, ty, Some(body)));
        } else {
            self.operators
                .infix
                .insert(s, Operator::binop_right(precedence, ty, Some(body)));
        }
    }

    fn get_inferred_type(&mut self) -> TypeSignature {
        TypeSignature::Infer(IntermediateType::new_any())
    }

    fn push_block(&mut self) {
        self.current_identifiers.push(HashSet::new());
    }

    fn push_identifier(&mut self, s: Rc<str>) {
        if self.current_identifiers.last().unwrap().contains(&s) {
            panic!("Redeclared identifier {}", s);
        }
        self.current_identifiers.last_mut().unwrap().insert(s);
    }

    fn pop_block(&mut self) {
        self.current_identifiers.pop().unwrap();
    }

    fn starts_c_macro(&self, s: &str) -> bool {
        !self.current_identifiers.iter().any(|t| t.contains(s))
            && (self.symbols.imported_macros.0.contains(s)
                || self.symbols.imported_macros.1.contains(s))
    }

    fn parse_unit(&mut self) -> Vec<Unit> {
        match &self.peek() {
            Some(Token::Operator(_, op)) if op.as_ref() == "@" => {
                read_token!(self, Token::Operator);
                self.parse_macro_unit()
            }
            _ => vec![match_first!(
                self.peek() => _t,
                default unexpected_token!(_t, self),

                Declaration => Unit::Declaration(Box::new(self.parse_declaration(true))),
                OperatorOverload => Unit::OperatorOverload(Box::new(self.parse_new_operator())),
                ImportFile => Unit::ImportFile(Box::new(self.parse_import())),
                Typedef => Unit::Typedef(Box::new(self.parse_typedef())),
            )],
        }
    }

    fn parse_declaration(&mut self, semi: bool) -> Declaration {
        let public = maybe_read_token!(self, Token::Pub).is_some();
        let mutable = match self.next() {
            Some(Token::Let(..)) => true,
            Some(Token::Const(..)) => false,
            Some(Token::Fun(..)) => {
                let inline = maybe_read_token!(self, Token::Inline).is_some();
                let ident = read_identifier_str!(self);
                self.push_identifier(ident.clone());
                let (params, return_type, block) = self.parse_lambda();
                return Declaration::Declaration(
                    DeclarationFlags {
                        public,
                        inline,
                        mutable: false,
                        static_: false,
                        typedef: false,
                    },
                    Box::new(TypeSignature::Function(
                        params.iter().cloned().map(From::from).collect(),
                        Box::new(return_type.clone()),
                    )),
                    vec![(ident, Some(Box::new(self.lambda_to_expr(params, return_type, block))))],
                );
            }
            t => unexpected_token!(t, self),
        };

        let flags = DeclarationFlags {
            public,
            mutable,
            inline: false,
            static_: false,
            typedef: false,
        };

        let mut decls = Vec::new();
        loop {
            let ident = read_identifier_str!(self).clone();
            self.push_identifier(ident.clone());

            if decls.len() == 0 && maybe_read_token!(self, Token::Colon).is_some() {
                let ty = Box::new(self.parse_type());
                let expr = match &self.peek() {
                    Some(Token::Operator(_, t)) if &**t == "=" => {
                        read_token!(self, Token::Operator);
                        Some(Box::new(self.parse_expression()))
                    }
                    _ => None,
                };
                let res = Declaration::Declaration(flags, ty, vec![(ident, expr)]);

                if semi {
                    read_token!(self, Token::SemiColon);
                }
                return res;
            }

            let expr = match &self.peek() {
                Some(Token::Operator(_, t)) if &**t == "=" => {
                    read_token!(self, Token::Operator);
                    Some(Box::new(self.parse_expression()))
                }
                _ => None,
            };
            decls.push((ident, expr));
            if maybe_read_token!(self, Token::Comma).is_none() {
                break;
            }
        }
        let ty = match self.peek() {
            Some(Token::Colon(_)) => {
                read_token!(self, Token::Colon);
                Box::new(self.parse_type())
            }
            _ => Box::new(self.get_inferred_type()),
        };
        let res = Declaration::Declaration(flags, ty, decls);

        if semi {
            read_token!(self, Token::SemiColon);
        }
        res
    }

    fn parse_type(&mut self) -> TypeSignature {
        self.parse_type_(true)
    }

    fn parse_type_(&mut self, maybe_fn: bool) -> TypeSignature {
        let ref_regex = Regex::new(r"^(&|&\?)+$").unwrap();
        let ty = match &self.peek() {
            /* int */
            /* int -> int */
            Some(Token::Identifier(..)) => {
                let t = read_identifier_str!(self);
                match t.as_ref() {
                    "void" => TypeSignature::Primitive(Primitive::Void),
                    "char" => TypeSignature::Primitive(Primitive::Char),
                    "float" => TypeSignature::Primitive(Primitive::Float),
                    "double" => TypeSignature::Primitive(Primitive::Double),
                    "int" | "long" => panic!("Use i32 and i64 instead of int/long"),
                    "__attribute__" => {
                        read_token!(self, Token::OpenParen);
                        let ty = self.parse_type();
                        let res = self.parse_attributes(ty);
                        read_token!(self, Token::CloseParen);
                        res
                    }
                    _ => {
                        let first = t.chars().next();
                        if first == Some('i') || first == Some('u') {
                            let (_, len) = t.split_at(1);
                            len.parse::<usize>()
                                .map(|size| match first {
                                    Some('i') => TypeSignature::Primitive(Primitive::Int(size)),
                                    Some('u') => TypeSignature::Primitive(Primitive::UInt(size)),
                                    _ => unreachable!(),
                                })
                                .unwrap_or_else(|_| TypeSignature::Plain(t))
                        } else {
                            TypeSignature::Plain(t)
                        }
                    }
                }
            }
            /* (foo, bar: int) -> int */
            /* (int, int) */
            Some(Token::OpenParen(..)) => self.parse_fn_or_tuple(maybe_fn),
            /* struct { foo: int, bar: int } */
            Some(Token::Struct(..)) => self.parse_struct(),
            /* enum { foo, bar(int) } */
            Some(Token::Enum(..)) => self.parse_enum(),
            /* [int] */
            Some(Token::OpenBracket(..)) => self.parse_array(),
            /* &int, &?int */
            Some(Token::Operator(_, t)) if ref_regex.is_match(&t) => self.parse_ptr(&t),
            t => unexpected_token!(t, self),
        };
        if maybe_fn {
            self.maybe_parse_fn(ty)
        } else {
            ty
        }
    }

    fn parse_attributes(&mut self, mut ty: TypeSignature) -> TypeSignature {
        let mut attrs = Vec::new();
        while let Some(Token::Comma(..)) = self.peek() {
            read_token!(self, Token::Comma);
            match self.peek() {
                Some(Token::Identifier(_, ref s)) if &**s == "vector_size" => {
                    read_token!(self, Token::Identifier);
                    read_token!(self, Token::OpenParen);
                    let e = self.parse_expression();
                    read_token!(self, Token::CloseParen);
                    ty = match ty {
                        TypeSignature::Primitive(p) => TypeSignature::Vector(p),
                        _ => panic!("Cannot convert non-primitive type {:?} to a vector", ty)
                    };
                    attrs.push(Attribute::Vector(e));
                }
                t => unimplemented!("{:?}", t)
            }
        }
        TypeSignature::Attribute(Box::new(ty), attrs)
    }

    fn parse_fn_or_tuple(&mut self, maybe_fn: bool) -> TypeSignature {
        let params = self.parse_param_list();
        match &self.peek() {
            Some(Token::Operator(_, t)) if &**t == "->" && maybe_fn => {
                read_token!(self, Token::Operator);
                let return_type = self.parse_type();
                TypeSignature::Function(params, Box::new(return_type))
            }
            _t => TypeSignature::Tuple(params.clone().into_iter().map(From::from).collect()),
        }
    }

    fn parse_struct(&mut self) -> TypeSignature {
        read_token!(self, Token::Struct);
        let name = maybe_read_token!(self, Token::Identifier);
        read_token!(self, Token::OpenBrace);
        let fields = self.parse_struct_list();
        read_token!(self, Token::CloseBrace);
        TypeSignature::Struct(name.as_ref().identifier_s().cloned(), fields)
    }

    fn parse_enum(&mut self) -> TypeSignature {
        read_token!(self, Token::Enum);
        let name = maybe_read_token!(self, Token::Identifier);
        read_token!(self, Token::OpenBrace);
        let fields = self.parse_enum_list();
        read_token!(self, Token::CloseBrace);
        TypeSignature::Enum(name.as_ref().identifier_s().cloned(), fields)
    }

    fn parse_array(&mut self) -> TypeSignature {
        read_token!(self, Token::OpenBracket);
        let ty = self.parse_type();
        let expr = match self.peek() {
            Some(Token::SemiColon(..)) => {
                read_token!(self, Token::SemiColon);
                Some(self.parse_expression())
            }
            _ => None,
        };
        let val = match expr.as_ref().map(|e| e.eval(&HashMap::new())) {
            Some(Ok(Literal::Integer(i))) => Some(TryFrom::try_from(i).unwrap()),
            Some(Err(_)) => None,
            Some(otherwise) => panic!("Not implemented: {:?}", otherwise),
            None => None,
        };
        read_token!(self, Token::CloseBracket);

        val.map(|i| TypeSignature::Array(Box::new(ty.clone()), Some(i)))
            .or_else(|| {
                expr.map(|e| TypeSignature::DynamicArray(Box::new(ty.clone()), Box::new(e)))
            })
            .unwrap_or_else(|| TypeSignature::Array(Box::new(ty), None))
    }

    fn parse_ptr(&mut self, op: &str) -> TypeSignature {
        read_token!(self, Token::Operator);
        let mut ty = self.parse_type_(false);
        let mut ref_iter = op.chars().rev();
        loop {
            match ref_iter.next() {
                Some('?') => {
                    assert_eq!(ref_iter.next(), Some('&'));
                    ty = TypeSignature::Pointer {
                        nullable: true,
                        ty: Box::new(ty),
                    };
                }
                Some('&') => {
                    ty = TypeSignature::Pointer {
                        nullable: false,
                        ty: Box::new(ty),
                    }
                }
                None => break,
                _ => unreachable!(),
            }
        }
        ty
    }

    fn maybe_parse_fn(&mut self, arg_ty: TypeSignature) -> TypeSignature {
        match &self.peek() {
            Some(Token::Operator(_, t)) if &**t == "->" => {
                read_token!(self, Token::Operator);
                let return_type = self.parse_type();
                TypeSignature::Function(
                    vec![Param::TypeOnly(Box::new(arg_ty))],
                    Box::new(return_type),
                )
            }
            _ => arg_ty,
        }
    }

    fn parse_param_list(&mut self) -> Vec<Param> {
        read_token!(self, Token::OpenParen);
        let res = self.parse_comma_delimited_to_vec(Self::parse_param);
        read_token!(self, Token::CloseParen);
        res
    }

    fn parse_param(&mut self) -> Option<Param> {
        match self.peek() {
            Some(Token::Identifier(_, ident)) => {
                let ty = self.parse_type();
                if let TypeSignature::Plain(_) = &ty {
                    match self.peek() {
                        Some(Token::Colon(_)) => {
                            self.next();
                            let ty = self.parse_type();
                            Some(Param::Param(ident.clone(), Box::new(ty)))
                        }
                        _ => Some(Param::TypeOnly(Box::new(TypeSignature::Plain(
                            ident.clone(),
                        )))),
                    }
                } else {
                    Some(Param::TypeOnly(Box::new(ty)))
                }
            }
            Some(_) => match_first!(
                self.peek() => _t,
                default None,

                TypeSignature => Some(Param::TypeOnly(Box::new(self.parse_type()))),),
            None => None,
        }
    }

    fn parse_struct_list(&mut self) -> Vec<StructField> {
        let vec = self.parse_comma_delimited_to_vec(Self::parse_struct_field);
        let mut names = HashSet::new();
        for StructField { name, .. } in &vec {
            if names.contains(&name) {
                panic!("Duplicate field name {}", name);
            }
            names.insert(name);
        }
        vec
    }

    fn parse_enum_list(&mut self) -> Vec<EnumField> {
        self.parse_comma_delimited_to_vec(Self::parse_enum_field)
    }

    fn parse_struct_field(&mut self) -> Option<StructField> {
        match self.peek() {
            Some(Token::Identifier(_, _)) => {
                let name = self.next().as_ref().identifier_s().unwrap().clone();
                read_token!(self, Token::Colon);
                let ty = Box::new(self.parse_type());
                Some(StructField {
                    name,
                    ty,
                    bitfield: None,
                })
            }
            _ => None,
        }
    }

    fn parse_enum_field(&mut self) -> Option<EnumField> {
        match &self.peek() {
            Some(Token::Identifier(_, _)) => {
                let name = self.next().as_ref().identifier_s().unwrap().clone();
                let value = match &self.peek() {
                    Some(Token::Operator(_, t)) if &**t == "=" => {
                        panic!("Not implemented");
                    }
                    _ => 0,
                };
                let ty = match self.peek() {
                    Some(Token::OpenParen(..)) => Some(self.parse_type()),
                    _ => None,
                };
                Some(EnumField { name, value, ty })
            }
            _ => None,
        }
    }

    fn parse_comma_delimited_to_vec<T>(&mut self, cb: fn(&mut Self) -> Option<T>) -> Vec<T> {
        let mut things = Vec::new();
        if let Some(p) = cb(self) {
            things.push(p);
        } else {
            return things;
        }
        while let Some(Token::Comma(..)) = self.peek() {
            read_token!(self, Token::Comma);
            match cb(self) {
                Some(p) => things.push(p),
                None => unexpected_token!(self.peek(), self),
            }
        }
        things
    }

    fn parse_new_operator(&mut self) -> OperatorOverload {
        read_token!(self, Token::NewOperator);
        let left_associative = match maybe_read_token!(self, Token::Identifier) {
            Some(Token::Identifier(i, t)) => match t.as_ref() {
                "left" => true,
                "right" => false,
                _ => unexpected_token!(Some(Token::Identifier(i, t.clone())), self),
            },
            None => true,
            Some(_) => unreachable!(),
        };
        let precedence = if let Token::Integer(i, t) = read_token!(self, Token::Integer) {
            usize::try_from(t).unwrap_or_else(|_| {
                println!("Unexpected signed integer");
                unexpected_token!(Some(Token::Integer(i, t)), self);
            })
        } else {
            unreachable!();
        };
        let op = if let Token::Operator(_, s) = read_token!(self, Token::Operator) {
            s
        } else {
            unreachable!();
        };
        let (params, return_type, block) = self.parse_lambda();
        let ty = TypeSignature::Function(
            params.iter().cloned().map(From::from).collect(),
            Box::new(return_type.clone()),
        );
        let body = self.lambda_to_expr(params, return_type, block);
        self.push_operator(
            left_associative,
            precedence,
            op.clone(),
            ty.clone(),
            body.clone(),
        );
        OperatorOverload::OperatorOverload(op, Box::new(ty), Box::new(body))
    }

    fn parse_import(&mut self) -> ImportFile {
        read_token!(self, Token::Import);
        let ffi = match self.peek() {
            Some(Token::OpenParen(..)) => {
                read_token!(self, Token::OpenParen);
                let ffi = if let Token::Identifier(_, s) = read_token!(self, Token::Identifier) {
                    s
                } else {
                    unreachable!();
                };
                read_token!(self, Token::CloseParen);
                Some(ffi)
            }
            _ => None,
        };
        let file = if let Token::StringLiteral(_, s) = read_token!(self, Token::StringLiteral) {
            s
        } else {
            unreachable!();
        };
        let content = (self.get_file)(FileQuery::new(
            self.current_file,
            &file,
            true,
            false,
            self.symbols.types.1
                .clone()
                .into_iter()
                .chain(self.symbols.imported_types.1.clone().into_iter())
                .collect(),
        ));
        content
            .declarations
            .unwrap()
            .into_iter()
            .for_each(|(name, decl)| {
                self.symbols.imported_declarations.0.insert(name, decl);
            });
        for (name, ty) in content.types.unwrap() {
            self.symbols.imported_types.0.insert(name.clone(), ty.clone());
            self.symbols.imported_types.1.push(name.clone());
        }
        self.symbols.imported_macros = content.c_macros;
        let mut buf = String::new();
        std::mem::swap(&mut buf, &mut self.define_buf);
        ImportFile::Import(file, ffi, buf)
    }

    fn parse_typedef(&mut self) -> Typedef {
        match self.peek() {
            Some(Token::Struct(..)) => {
                read_token!(self, Token::Struct);
                let name = read_identifier_str!(self);
                read_token!(self, Token::OpenBrace);
                let fields = self.parse_struct_list();
                read_token!(self, Token::CloseBrace);
                self.symbols.types.0.insert(
                    name.clone(),
                    (TypeSignature::Struct(Some(name.clone()), fields.clone()), None),
                );
                self.symbols.types.1.push(name.clone());
                Typedef::Struct(name, fields)
            }
            Some(Token::Enum(..)) => unimplemented!(),
            Some(Token::Type(..)) => {
                read_token!(self, Token::Type);
                let name = read_identifier_str!(self);
                let op = read_token!(self, Token::Operator);
                match op {
                    Token::Operator(_, ref s) if &**s == "=" => {},
                    t => unexpected_token!(Some(t), self)
                }
                let ty = self.parse_type();
                read_token!(self, Token::SemiColon);
                self.symbols.types.0.insert(name.clone(), (ty.clone(), Some(name.clone())));
                self.symbols.types.1.push(name.clone());
                Typedef::Alias(true, name, Box::new(ty))
            }
            _ => unexpected_token!(self.peek(), self),
        }
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_expression_(1)
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_expression_(&mut self, precedence: usize) -> Expression {
        let expr = match &self.peek() {
            Some(Token::Operator(_, op)) if op.as_ref() == "@" => {
                read_token!(self, Token::Operator);
                self.parse_macro_expression()
            }
            Some(Token::Operator(_, op)) => match self.operators.unary.get(op).cloned() {
                Some(ref n) if n.left_associative => {
                    assert_eq!(n.left_associative, true);
                    read_token!(self, Token::Operator);
                    let exprs = (0..n.param_count)
                        .map(|_| self.parse_expression_(n.precedence))
                        .collect();
                    Expression::Unary(op.clone(), ExprList::List(exprs))
                }
                _ => panic!("Unknown prefix operator: {:?}", op),
            },
            Some(Token::Sizeof(..)) => {
                read_token!(self, Token::Sizeof);
                read_token!(self, Token::OpenParen);
                let res = match self.parse_type_or_expression() {
                    Ok(ty) => Expression::Sizeof(Sizeof::Type(ty)),
                    Err(expr) => Expression::Sizeof(Sizeof::Expression(expr)),
                };
                read_token!(self, Token::CloseParen);
                res
            }
            Some(Token::Identifier(_, s)) => {
                if self.starts_c_macro(&s) {
                    self.parse_macro_expression()
                } else {
                    let t = self.next().as_ref().identifier_s().unwrap().clone();
                    let ty = self.get_ty(&t).cloned();
                    match self.peek() {
                        Some(Token::OpenBrace(..))
                            if ty.as_ref().map(|t| t.is_compound(&HashMap::new()))
                                == Some(true) =>
                        {
                            let fields = self.parse_initialization_fields(&t);
                            Expression::StructInitialization(t, fields)
                        }
                        Some(Token::OpenBrace(..)) if ty.is_some() => {
                            let fields = self.parse_vector_initialization_fields();
                            Expression::VectorInitialization(t, fields)
                        }
                        _ => Expression::Identifier(t),
                    }
                }
            }
            Some(Token::Integer(..))
            | Some(Token::Float(..))
            | Some(Token::StringLiteral(..))
            | Some(Token::Char(..)) => Expression::Literal(self.parse_literal()),
            Some(Token::If(..)) | Some(Token::While(..)) | Some(Token::Do(..)) => {
                Expression::BlockExpression(Box::new(self.parse_block_expression()))
            }
            Some(Token::OpenParen(..)) => {
                read_token!(self, Token::OpenParen);
                let expr = Expression::Expression(Box::new(self.parse_expression()));
                read_token!(self, Token::CloseParen);
                expr
            }
            Some(Token::OpenBrace(..)) => {
                Expression::BlockExpression(Box::new(self.parse_block_expression()))
            }
            Some(Token::OpenBracket(..)) => {
                Expression::ArrayLiteral(self.parse_array_literal())
            }
            Some(Token::Fun(..)) => {
                read_token!(self, Token::Fun);
                let (params, return_type, block) = self.parse_lambda();
                Expression::Lambda(Lambda::Lambda(params, return_type, block))
            }
            t => unexpected_token!(t, self),
        };
        self.parse_expression__(expr, precedence)
    }

    fn parse_type_or_expression(&mut self) -> Result<Box<TypeSignature>, Box<Expression>> {
        // remove stuff until the first identifier, then push the stuff back, and parse
        let mut buf = Vec::new();
        let ident;

        loop {
            match self.next() {
                None => panic!(),
                Some(t@Token::Identifier(..)) => {
                    buf.push(t.clone());
                    ident = t;
                    break;
                }
                Some(t) => buf.push(t)
            }
        }

        let mut new_iter = VecDeque::new();
        std::mem::swap(&mut self.iter, &mut new_iter);

        self.iter = buf.into_iter()
            .chain(new_iter.into_iter()).collect();

        match &ident {
            Token::Identifier(_, s) if self.symbols.types.0.contains_key(s) || s.as_ref() == "char" => Ok(Box::new(self.parse_type())),
            _ => Err(Box::new(self.parse_expression())),
        }
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_expression__(&mut self, mut expr: Expression, precedence: usize) -> Expression {
        loop {
            match self.peek() {
                Some(Token::As(..)) => {
                    read_token!(self, Token::As);
                    let ty = self.parse_type();
                    expr = Expression::Cast(Box::new(expr), ty);
                }
                Some(Token::OpenBracket(..)) => {
                    read_token!(self, Token::OpenBracket);
                    let inner_expr = Box::new(self.parse_expression());
                    read_token!(self, Token::CloseBracket);
                    expr = Expression::ArrayAccess(Box::new(expr), inner_expr);
                }
                Some(Token::Dot(..)) => {
                    read_token!(self, Token::Dot);
                    let ident = read_identifier_str!(self);
                    expr = Expression::StructAccess(Box::new(expr), ident);
                }
                Some(Token::OpenParen(..)) => {
                    expr = Expression::Call(Box::new(expr), self.parse_args())
                }
                Some(Token::Operator(_, op)) => match self.operators.infix.get(&op).cloned() {
                    Some(ref n) if precedence <= n.precedence => {
                        let mut left = vec![expr];
                        read_token!(self, Token::Operator);
                        if op.as_ref() == "?" {
                            let right_t = self.parse_expression_(n.precedence);
                            read_token!(self, Token::Colon);
                            let right_f = self.parse_expression_(n.precedence);
                            left.append(&mut vec![right_t, right_f]);
                            expr = Expression::Op(op.clone(), ExprList::List(left));
                        } else {
                            let mut tail = (1..n.param_count)
                                .map(|_| self.parse_expression_(n.precedence))
                                .collect();
                            left.append(&mut tail);
                            expr = Expression::Op(op.clone(), ExprList::List(left));
                        }
                    }
                    Some(_) => break,
                    None => match self.operators.postfix.get(&op) {
                        Some(_) => {
                            read_token!(self, Token::Operator);
                            expr = Expression::PostFix(Box::new(expr), op.clone());
                        }
                        _ => panic!("Unknown operator: {}", op),
                    },
                },
                _ => break,
            }
        }
        expr
    }

    fn parse_macro_expression(&mut self) -> Expression {
        let mut result = self.parse_macro();

        assert_eq!(result.len(), 1);
        match result.remove(0) {
            MacroExpansion::Expression(e) => e,
            _ => panic!(),
        }
    }

    fn parse_macro_unit(&mut self) -> Vec<Unit> {
        self.parse_macro()
            .into_iter()
            .flat_map(|result| {
                match result {
                    MacroExpansion::Expression(e) => {
                        panic!("Macro expanded into an expression {:?}", e)
                    }
                    MacroExpansion::Statement(s) => match s {
                        Statement::Declaration(d) => Some(Unit::Declaration(d)),
                        Statement::Pragma(_s) => unimplemented!(),
                        _ => panic!("Not supported"),
                    },
                    MacroExpansion::Type(ty) => match ty {
                        TypeSignature::Struct(Some(n), fields) => {
                            Some(Unit::Typedef(Box::new(Typedef::Struct(n, fields))))
                        }
                        TypeSignature::Enum(Some(n), fields) => {
                            Some(Unit::Typedef(Box::new(Typedef::Enum(n, fields))))
                        }
                        _ => None,
                    },
                    MacroExpansion::Typedef(typedef) => {
                        if let Typedef::Alias(_, name, ty) = typedef.clone() {
                            self.symbols.types.0.insert(name.clone(), (*ty, Some(name.clone())));
                            self.symbols.types.1.push(name.clone());
                        }
                        Some(Unit::Typedef(Box::new(typedef)))
                    }
                }
                .into_iter()
            })
            .collect()
    }

    fn parse_macro(&mut self) -> Vec<MacroExpansion> {
        let tys = self.symbols.types.1.clone().into_iter()
            .chain(self.symbols.imported_types.1.clone().into_iter())
            .collect();
        match self.peek() {
            Some(Token::StringLiteral(..)) => {
                if let Token::StringLiteral(_, s) = read_token!(self, Token::StringLiteral) {
                    if s.contains("#define") {
                        self.define_buf.push_str(&s);
                    }
                    (self.expand)(s.to_string(), tys)
                } else {
                    unreachable!();
                }
            }
            Some(Token::Identifier(_, s)) => {
                if self.symbols.imported_macros.0.contains(&s) {
                    let s = read_identifier_str!(self);
                    (self.expand)(s.to_string(), tys)
                } else if self.symbols.imported_macros.1.contains(&s) {
                    let s = read_identifier_str!(self);
                    let args = self.parse_args();

                    (self.expand_call)(s.to_string(), args, tys)
                } else {
                    unimplemented!();
                }
            }
            t => unexpected_token!(t, self),
        }
    }

    fn parse_lambda(&mut self) -> (Vec<LambdaParam>, TypeSignature, Block) {
        let params = self.parse_lambda_param_list();
        let return_type = match &self.peek() {
            Some(Token::Operator(_, t)) if &**t == "->" => {
                read_token!(self, Token::Operator);
                self.parse_type()
            }
            _ => self.get_inferred_type(),
        };
        let block = match &self.peek() {
            Some(Token::Operator(_, t)) if &**t == "=>" => {
                read_token!(self, Token::Operator);
                let expr = self.parse_expression();
                Block::Statements(vec![Statement::Return(Some(Box::new(expr)))])
            }
            _ => self.parse_block(),
        };
        (params, return_type, block)
    }

    fn parse_lambda_param_list(&mut self) -> Vec<LambdaParam> {
        read_token!(self, Token::OpenParen);
        let res = self.parse_comma_delimited_to_vec(Self::parse_lambda_param);
        read_token!(self, Token::CloseParen);
        res
    }

    fn parse_lambda_param(&mut self) -> Option<LambdaParam> {
        match self.peek() {
            Some(Token::Identifier(..)) => {
                let ident = self.next().as_ref().identifier_s().unwrap().clone();
                match self.peek() {
                    Some(Token::Colon(_)) => {
                        read_token!(self, Token::Colon);
                        let ty = self.parse_type();
                        Some(LambdaParam::LambdaParam(ident, Box::new(ty)))
                    }
                    _ => Some(LambdaParam::LambdaParam(
                        ident,
                        Box::new(self.get_inferred_type()),
                    )),
                }
            }
            _ => None,
        }
    }

    fn lambda_to_expr(
        &self,
        params: Vec<LambdaParam>,
        return_type: TypeSignature,
        block: Block,
    ) -> Expression {
        Expression::Lambda(Lambda::Lambda(
            params,
            return_type,
            block,
        ))
    }

    fn parse_block_expression(&mut self) -> BlockExpression {
        match self.peek() {
            Some(Token::If(..)) => self.parse_if_expr(),
            Some(Token::While(..)) => self.parse_while_expr(),
            Some(Token::Do(..)) => self.parse_do_while_expr(),
            Some(Token::For(..)) => self.parse_for_expr(),
            Some(Token::OpenBrace(..)) => BlockExpression::Block(self.parse_block()),
            t => unexpected_token!(t, self),
        }
    }

    fn parse_array_literal(&mut self) -> Vec<Expression> {
        let mut exprs = Vec::new();

        read_token!(self, Token::OpenBracket);

        loop {
            match self.peek() {
                Some(Token::CloseBracket(..)) => break,
                _ => {
                    exprs.push(self.parse_expression());
                    if let Some(Token::Comma(..)) = self.peek() {
                        read_token!(self, Token::Comma);
                        continue;
                    } else {
                        break;
                    }
                }
            }
        }

        read_token!(self, Token::CloseBracket);
        exprs
    }

    fn parse_while_expr(&mut self) -> BlockExpression {
        // while expr block [else block]
        read_token!(self, Token::While);
        let expr = self.parse_expression();
        let block = self.parse_block();
        let else_block = match self.peek() {
            Some(Token::Else(..)) => {
                read_token!(self, Token::Else);
                Some(Box::new(self.parse_block()))
            }
            _ => None,
        };
        BlockExpression::While(Box::new(expr), Box::new(block), else_block, false)
    }

    fn parse_do_while_expr(&mut self) -> BlockExpression {
        // do block while expr
        read_token!(self, Token::Do);
        let block = self.parse_block();
        read_token!(self, Token::While);
        let expr = self.parse_expression();
        let else_block = None;
        BlockExpression::While(Box::new(expr), Box::new(block), else_block, true)
    }

    fn parse_for_expr(&mut self) -> BlockExpression {
        // for (maybe-statement ; maybe-expression ; maybe-expression) block [else block]
        read_token!(self, Token::For);

        read_token!(self, Token::OpenParen);

        self.push_block();

        let init = match self.peek() {
            Some(Token::SemiColon(..)) => None,
            _ => Some(Box::new(self.parse_statement(false))),
        };
        read_token!(self, Token::SemiColon);

        let cond = match self.peek() {
            Some(Token::SemiColon(..)) => None,
            _ => Some(Box::new(self.parse_expression())),
        };
        read_token!(self, Token::SemiColon);

        let post_loop = match self.peek() {
            Some(Token::CloseParen(..)) => None,
            _ => Some(Box::new(self.parse_expression())),
        };

        read_token!(self, Token::CloseParen);

        let block = self.parse_block();
        let else_block = match self.peek() {
            Some(Token::Else(..)) => {
                read_token!(self, Token::Else);
                Some(Box::new(self.parse_block()))
            }
            _ => None,
        };
        self.pop_block();
        BlockExpression::For(init, cond, post_loop, Box::new(block), else_block)
    }

    fn parse_literal(&mut self) -> Literal {
        match self.peek() {
            Some(Token::Integer(_, i)) => {
                read_token!(self, Token::Integer);
                Literal::Integer(i)
            }
            Some(Token::Float(_, s)) => {
                read_token!(self, Token::Float);
                Literal::Float(s)
            }
            Some(Token::StringLiteral(_, s)) => {
                read_token!(self, Token::StringLiteral);
                Literal::StringLiteral(s)
            }
            Some(Token::Char(_, c)) => {
                read_token!(self, Token::Char);
                Literal::Char(c)
            }
            t => unexpected_token!(t, self),
        }
    }

    fn parse_if_expr(&mut self) -> BlockExpression {
        // if expr block [elif expr block ]* [else block]
        read_token!(self, Token::If);
        let mut choices = Vec::new();
        let expr = self.parse_expression();
        let block = self.parse_block();
        let mut or_else = None;
        choices.push((Box::new(expr), Box::new(block)));
        loop {
            match self.peek() {
                Some(Token::Elif(..)) => {
                    read_token!(self, Token::Elif);
                    let expr = self.parse_expression();
                    let block = self.parse_block();
                    choices.push((Box::new(expr), Box::new(block)));
                }
                Some(Token::Else(..)) => {
                    read_token!(self, Token::Else);
                    let block = self.parse_block();
                    or_else = Some(Box::new(block));
                    break;
                }
                _ => break,
            }
        }
        BlockExpression::If(choices, or_else)
    }

    fn parse_block(&mut self) -> Block {
        if maybe_read_token!(self, Token::SemiColon).is_some() {
            return Block::Statements(vec![]);
        }
        self.push_block();
        read_token!(self, Token::OpenBrace);
        let mut stmts = Vec::new();
        loop {
            match self.peek() {
                Some(Token::CloseBrace(..)) => {
                    break;
                }
                Some(Token::SemiColon(..)) => {
                    read_token!(self, Token::SemiColon);
                }
                Some(_) => {
                    stmts.push(self.parse_statement(true));
                }
                t => unexpected_token!(t, self),
            }
        }
        read_token!(self, Token::CloseBrace);
        self.pop_block();
        Block::Statements(stmts)
    }

    fn parse_macro_as_statement(&mut self) -> Statement {
        let mut res = self
            .parse_macro()
            .into_iter()
            .map(|m| match m {
                MacroExpansion::Expression(e) => Statement::Expression(Box::new(e)),
                MacroExpansion::Statement(s) => s,
                MacroExpansion::Type(_t) => unimplemented!(),
                MacroExpansion::Typedef(_t) => unimplemented!(),
            })
            .collect::<Vec<_>>();
        if res.len() == 1 {
            match res.remove(0) {
                Statement::Expression(e) => {
                    Statement::Expression(Box::new(self.parse_expression__(*e, 1)))
                }
                otherwise => otherwise,
            }
        } else {
            Statement::BlockExpression(Box::new(BlockExpression::Block(Block::Statements(res))))
        }
    }

    #[allow(unreachable_patterns)]
    fn parse_statement(&mut self, semi: bool) -> Statement {
        match &self.peek() {
            Some(Token::Operator(_, op)) if op.as_ref() == "@" => {
                read_token!(self, Token::Operator);
                return self.parse_macro_as_statement();
            }
            Some(Token::Identifier(_, ident)) if self.starts_c_macro(ident) => {
                return self.parse_macro_as_statement()
            }
            _ => {}
        }
        let res = match_first!(
            self.peek() => _t,
            default unexpected_token!(_t, self),

            // directly return the block_expr since it doesn't need a semicolon
            BlockExpression => return Statement::BlockExpression(Box::new(self.parse_block_expression())),
            Declaration => return Statement::Declaration(Box::new(self.parse_declaration(semi))),
            PragmaStatement => return Statement::Pragma(self.parse_pragma()),
            // Expressions can contain BlockExpressions, so this is
            // partially unreachable
            Expression => Statement::Expression(Box::new(self.parse_expression())),
            JumpStatement => Statement::Jump(self.parse_jump()),
            ReturnStatement => {
                read_token!(self, Token::Return);
                let expr = match self.peek() {
                    Some(Token::SemiColon(..)) => None,
                    _ => Some(Box::new(self.parse_expression()))
                };
                Statement::Return(expr)
            },
        );
        if semi {
            read_token!(self, Token::SemiColon);
        }
        res
    }

    fn parse_pragma(&mut self) -> Rc<str> {
        read_token!(self, Token::Pragma);
        if let Token::StringLiteral(_, s) = read_token!(self, Token::StringLiteral) {
            s
        } else {
            unreachable!();
        }
    }

    fn parse_jump(&mut self) -> JumpStatement {
        match self.peek() {
            Some(Token::Break(..)) => {
                read_token!(self, Token::Break);
                JumpStatement::Break
            }
            Some(Token::Continue(..)) => {
                read_token!(self, Token::Continue);
                JumpStatement::Continue
            }
            t => unexpected_token!(t, self),
        }
    }

    fn parse_args(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();

        read_token!(self, Token::OpenParen);
        loop {
            match self.peek() {
                Some(Token::CloseParen(..)) => {
                    read_token!(self, Token::CloseParen);
                    break;
                }
                Some(_) => {
                    args.push(self.parse_expression());
                    match self.peek() {
                        Some(Token::CloseParen(..)) => {
                            read_token!(self, Token::CloseParen);
                            break;
                        }
                        Some(Token::Comma(..)) => {
                            read_token!(self, Token::Comma);
                        }
                        t => unexpected_token!(t, self),
                    }
                }
                t => unexpected_token!(t, self),
            }
        }

        args
    }

    #[allow(clippy::cognitive_complexity)]
    fn parse_initialization_fields(&mut self, ty: &Rc<str>) -> Vec<StructInitializationField> {
        if let TypeSignature::Struct(..) = &self.symbols.types.0[ty].0 {
        } else {
            panic!("Cannot instantiate non-struct type {} as struct", ty);
        };

        let mut fields = Vec::new();

        read_token!(self, Token::OpenBrace);
        loop {
            match self.peek() {
                Some(Token::CloseBrace(..)) => {
                    read_token!(self, Token::CloseBrace);
                    break;
                }
                Some(_) => {
                    let expr = self.parse_expression();
                    match self.peek() {
                        Some(Token::Colon(i)) => {
                            if let Expression::Identifier(ident) = expr
                            {
                                read_token!(self, Token::Colon);
                                let expr = self.parse_expression();

                                fields.push(StructInitializationField::StructInitializationField(
                                    Some(ident),
                                    Box::new(expr),
                                ));
                            } else {
                                unexpected_token!(Some(&Token::Colon(i)), self);
                            }

                            match self.peek() {
                                Some(Token::Comma(..)) => {
                                    read_token!(self, Token::Comma);
                                }
                                Some(Token::CloseBrace(..)) => {
                                    read_token!(self, Token::CloseBrace);
                                    break;
                                }
                                t => unexpected_token!(t, self),
                            }
                        }
                        Some(Token::Comma(..)) => {
                            fields.push(StructInitializationField::StructInitializationField(
                                None,
                                Box::new(expr),
                            ));
                            read_token!(self, Token::Comma);
                        }
                        Some(Token::CloseBrace(..)) => {
                            fields.push(StructInitializationField::StructInitializationField(
                                None,
                                Box::new(expr),
                            ));
                            read_token!(self, Token::CloseBrace);
                            break;
                        }
                        t => unexpected_token!(t, self),
                    }
                }
                t => unexpected_token!(t, self),
            }
        }

        fields
    }

    fn parse_vector_initialization_fields(&mut self) -> Vec<Expression> {
        read_token!(self, Token::OpenBrace);
        let fields = self.parse_comma_delimited_to_vec(Self::parse_vector_initialization_field);
        read_token!(self, Token::CloseBrace);
        fields
    }

    fn parse_vector_initialization_field(&mut self) -> Option<Expression> {
        match self.peek() {
            Some(Token::CloseBrace(..)) => None,
            _ => Some(self.parse_expression()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use purkkatoken::token::Token;

    fn to_token(prec: &OperatorMap, s: &str) -> Token {
        prec.get(s)
            .map(|_| Token::Operator(0, From::from(s)))
            .or_else(|| {
                if s == ":" {
                    Some(Token::Colon(0))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| Token::Integer(0, s.parse().unwrap()))
    }

    fn get_decl_ident(tree: &S) -> Rc<str> {
        Some(tree)
            .translation_unit()
            .units()
            .and_then(|t| t.get(0))
            .declaration()
            .map(|Declaration::Declaration(_, _, decls)| {
                assert_eq!(decls.len(), 1);
                decls[0].0.clone()
            })
            .unwrap()
    }

    fn get_decl_expr(tree: &S) -> Expression {
        Some(tree)
            .translation_unit()
            .units()
            .and_then(|t| t.get(0))
            .declaration()
            .map(|Declaration::Declaration(_, _, decls)| {
                 assert_eq!(decls.len(), 1);
                 *decls[0].1.as_ref().unwrap().clone()
            })
            .unwrap()
    }

    macro_rules! eval_bin {
        ($list:ident, $op:tt) => {
            {
                assert_eq!($list.len(), 2);
                eval_tree(&$list[0]) $op eval_tree(&$list[1])
            }
        }
    }

    fn eval_tree(expr: &Expression) -> i128 {
        match expr {
            Expression::Literal(Literal::Integer(
                e,
            )) => *e,
            Expression::Cast(..) => unreachable!(),
            Expression::Op(op, ExprList::List(list)) => match op.as_ref() {
                "+" => eval_bin!(list, +),
                "-" => eval_bin!(list, -),
                "*" => eval_bin!(list, *),
                "/" => eval_bin!(list, /),
                "&" => eval_bin!(list, &),
                "|" => eval_bin!(list, |),
                "^" => eval_bin!(list, ^),
                "?" => {
                    assert_eq!(list.len(), 3);
                    if eval_tree(&list[0]) != 0 {
                        eval_tree(&list[1])
                    } else {
                        eval_tree(&list[2])
                    }
                }
                _ => unreachable!(),
            },
            Expression::Unary(op, ExprList::List(list)) => match op.as_ref() {
                "-" => -eval_tree(&list[0]),
                "~" => !eval_tree(&list[0]),
                _ => unreachable!(),
            },
            Expression::PostFix(expr, op) => match op.as_ref() {
                "++" => eval_tree(&expr),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn check(expr: &str, expected: i128) {
        let unary = default_unary_ops();
        let postfix = default_postfix_ops();
        let infix = default_bin_ops();
        let both = unary
            .clone()
            .into_iter()
            .chain(infix.clone().into_iter())
            .collect();
        let vec: Vec<Token> = expr.split(' ').map(|t| to_token(&both, t)).collect();
        let mut context = ParseContext {
            operators: Operators {
                infix,
                unary,
                postfix,
            },
            symbols: Symbols::default(),
            current_identifiers: vec![HashSet::new()],

            iter: From::from(vec),
            fragment: &FragmentIterator::new("", ""),
            sources: &Vec::new(),

            current_file: "",
            get_file: &|_| panic!(),
            expand: &|_, _| panic!(),
            expand_call: &|_, _, _| panic!(),

            define_buf: String::new()
        };
        let result = eval_tree(&context.parse_expression());
        println!("{} = {} (expected: {})", expr, result, expected);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_expr_and_eval() {
        check("0", 0);
        check("- 1", -1);
        check("1 + 1", 2);
        check("1 & ~ 1", 0);
        check("1 + - 1", 0);
        check("1 * - 1", -1);
        check("1 + 2 * 2", 5);
        check("1 - 2 * 2", -3);
        check("1 ? 2 : 3", 2);
        check("0 ? 2 : 3", 3);
        check("1 ? 2 : 0 ? 3 : 4", 2); // no horses here
        check("0 ? 2 : 1 ? 3 : 4", 3);
        check("1 ? 2 : 1 ? 3 : 4", 2);
        check("0 ? 2 : 0 ? 3 : 4", 4);

        check("0 ++", 0);

        check("1 & 2", 0);
        check("1 & 3 | 4", 5);
        check("1 | 3 & 4", 1);
    }

    fn test_parse(list: Vec<Token>) -> S {
        parse(
            From::from(list),
            &vec![],
            &FragmentIterator::new("", ""),
            "",
            &|_| panic!(),
            &|_, _| panic!(),
            &|_, _, _| panic!(),
        )
        .0
    }

    #[test]
    fn parse_fn_empty() {
        let s = test_parse(vec![
            Token::Let(0),
            Token::Identifier(1, From::from("bar")),
            Token::Operator(2, From::from("=")),
            Token::Identifier(3, From::from("foo")),
            Token::OpenParen(4),
            Token::CloseParen(5),
            Token::SemiColon(6),
        ]);
        assert_eq!(get_decl_ident(&s), From::from("bar"));
        assert_eq!(
            get_decl_expr(&s),
            Expression::Call(
                Box::new(Expression::Identifier(From::from("foo"))),
                vec![]
            )
        );
    }

    #[test]
    fn parse_fn_one_arg() {
        let s = test_parse(vec![
            Token::Let(0),
            Token::Identifier(1, From::from("bar")),
            Token::Operator(2, From::from("=")),
            Token::Identifier(3, From::from("foo")),
            Token::OpenParen(4),
            Token::Identifier(5, From::from("asd")),
            Token::CloseParen(6),
            Token::SemiColon(7),
        ]);
        assert_eq!(get_decl_ident(&s), From::from("bar"));
        assert_eq!(
            get_decl_expr(&s),
            Expression::Call(
                Box::new(Expression::Identifier(From::from("foo"))),
                vec![Expression::Identifier(From::from("asd"))]
            )
        );
    }

    #[test]
    fn parse_fn_two_args() {
        let s = test_parse(vec![
            Token::Let(0),
            Token::Identifier(1, From::from("bar")),
            Token::Operator(2, From::from("=")),
            Token::Identifier(3, From::from("foo")),
            Token::OpenParen(4),
            Token::Identifier(5, From::from("asd")),
            Token::Comma(6),
            Token::Identifier(7, From::from("qwe")),
            Token::CloseParen(8),
            Token::SemiColon(9),
        ]);
        assert_eq!(get_decl_ident(&s), From::from("bar"));
        assert_eq!(
            get_decl_expr(&s),
            Expression::Call(
                Box::new(Expression::Identifier(From::from("foo"))),
                vec![
                    Expression::Identifier(From::from("asd")),
                    Expression::Identifier(From::from("qwe"))
                ]
            )
        );
    }

    #[test]
    fn parse_fn_three_args() {
        let s = test_parse(vec![
            Token::Let(1),
            Token::Identifier(2, From::from("bar")),
            Token::Operator(3, From::from("=")),
            Token::Identifier(4, From::from("foo")),
            Token::OpenParen(5),
            Token::Identifier(6, From::from("asd")),
            Token::Comma(7),
            Token::Identifier(8, From::from("qwe")),
            Token::Comma(9),
            Token::Identifier(10, From::from("aoeu")),
            Token::CloseParen(11),
            Token::SemiColon(12),
        ]);
        assert_eq!(get_decl_ident(&s), From::from("bar"));
        assert_eq!(
            get_decl_expr(&s),
            Expression::Call(
                Box::new(Expression::Identifier(From::from("foo"))),
                vec![
                    Expression::Identifier(From::from("asd")),
                    Expression::Identifier(From::from("qwe")),
                    Expression::Identifier(From::from("aoeu"))
                ]
            )
        );
    }

    #[test]
    fn parse_ty() {
        let s = test_parse(vec![
            Token::Let(0),
            Token::Identifier(1, From::from("bar")),
            Token::Colon(2),
            Token::Operator(3, From::from("&")),
            Token::Identifier(4, From::from("i32")),
            Token::SemiColon(9),
        ]);
        assert_eq!(
            s,
            S::TranslationUnit(TranslationUnit::Units(vec![Unit::Declaration(Box::new(
                Declaration::Declaration(
                    DeclarationFlags {
                        public: false,
                        inline: false,
                        mutable: true,
                        static_: false,
                        typedef: false
                    },
                    Box::new(TypeSignature::Pointer {
                        nullable: false,
                        ty: Box::new(TypeSignature::Primitive(Primitive::Int(32)))
                    },),
                    vec![(From::from("bar"), None)]
                )
            ))]))
        );
    }
}
