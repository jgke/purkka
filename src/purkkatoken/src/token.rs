use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    // Literals
    Identifier(usize, Rc<str>),
    Integer(usize, i128),
    Float(usize, Rc<str>),
    StringLiteral(usize, Rc<str>),
    Char(usize, char),
    Operator(usize, Rc<str>),

    // Delimiters: ( ) { } [ ] . ... , : ;
    OpenParen(usize),
    CloseParen(usize),
    OpenBrace(usize),
    CloseBrace(usize),
    OpenBracket(usize),
    CloseBracket(usize),
    Dot(usize),
    Variadic(usize),
    Comma(usize),
    Colon(usize),
    SemiColon(usize),

    // Keywords
    Pub(usize),
    Const(usize),
    Static(usize),
    Fun(usize),
    Let(usize),
    If(usize),
    Elif(usize),
    Else(usize),
    While(usize),
    For(usize),
    Type(usize),
    Struct(usize),
    Enum(usize),
    Return(usize),
    NewOperator(usize),
    As(usize),

    Import(usize),
}

use Token::*;

type TokenConstructor = fn(usize) -> Token;

pub static TOKEN_TYPES: &[(&str, TokenConstructor)] = &[
    (":", Colon),
    (";", SemiColon),
    ("...", Variadic),
    (".", Dot),
    (",", Comma),
    ("(", OpenParen),
    (")", CloseParen),
    ("{", OpenBrace),
    ("}", CloseBrace),
    ("[", OpenBracket),
    ("]", CloseBracket),
];

pub static KEYWORDS: &[(&str, TokenConstructor)] = &[
    ("pub", Pub),
    ("const", Const),
    ("static", Static),
    ("fun", Fun),
    ("let", Let),
    ("if", If),
    ("elif", Elif),
    ("else", Else),
    ("while", While),
    ("for", For),
    ("type", Type),
    ("struct", Struct),
    ("enum", Enum),
    ("return", Return),
    ("import", Import),
    ("operator", NewOperator),
    ("as", As),
];

impl Token {
    pub fn get_num(&self) -> usize {
        *match self {
            Identifier(i, ..) => i,
            Integer(i, ..) => i,
            Float(i, ..) => i,
            StringLiteral(i, ..) => i,
            Char(i, ..) => i,
            Operator(i, ..) => i,
            OpenParen(i, ..) => i,
            CloseParen(i, ..) => i,
            OpenBrace(i, ..) => i,
            CloseBrace(i, ..) => i,
            OpenBracket(i, ..) => i,
            CloseBracket(i, ..) => i,
            Dot(i, ..) => i,
            Variadic(i, ..) => i,
            Comma(i, ..) => i,
            Colon(i, ..) => i,
            SemiColon(i, ..) => i,
            Pub(i, ..) => i,
            Const(i, ..) => i,
            Static(i, ..) => i,
            Fun(i, ..) => i,
            Let(i, ..) => i,
            If(i, ..) => i,
            Elif(i, ..) => i,
            Else(i, ..) => i,
            While(i, ..) => i,
            For(i, ..) => i,
            Type(i, ..) => i,
            Struct(i, ..) => i,
            Enum(i, ..) => i,
            Return(i, ..) => i,
            NewOperator(i, ..) => i,
            As(i, ..) => i,
            Import(i, ..) => i,
        }
    }
}
