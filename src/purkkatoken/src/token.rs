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
    As(usize),
    Break(usize),
    Const(usize),
    Do(usize),
    Elif(usize),
    Else(usize),
    Enum(usize),
    For(usize),
    Fun(usize),
    If(usize),
    Import(usize),
    Inline(usize),
    Let(usize),
    NewOperator(usize),
    Pragma(usize),
    Pub(usize),
    Return(usize),
    Sizeof(usize),
    Static(usize),
    Struct(usize),
    Type(usize),
    While(usize),
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
    ("as", As),
    ("break", Break),
    ("const", Const),
    ("do", Do),
    ("elif", Elif),
    ("else", Else),
    ("enum", Enum),
    ("for", For),
    ("fun", Fun),
    ("if", If),
    ("import", Import),
    ("inline", Inline),
    ("let", Let),
    ("operator", NewOperator),
    ("pragma", Pragma),
    ("pub", Pub),
    ("return", Return),
    ("sizeof", Sizeof),
    ("static", Static),
    ("struct", Struct),
    ("type", Type),
    ("while", While),
];

impl Token {
    pub fn get_num(&self) -> usize {
        *match self {
            As(i, ..) => i,
            Break(i, ..) => i,
            Char(i, ..) => i,
            CloseBrace(i, ..) => i,
            CloseBracket(i, ..) => i,
            CloseParen(i, ..) => i,
            Colon(i, ..) => i,
            Comma(i, ..) => i,
            Const(i, ..) => i,
            Do(i, ..) => i,
            Dot(i, ..) => i,
            Elif(i, ..) => i,
            Else(i, ..) => i,
            Enum(i, ..) => i,
            Float(i, ..) => i,
            For(i, ..) => i,
            Fun(i, ..) => i,
            Identifier(i, ..) => i,
            If(i, ..) => i,
            Import(i, ..) => i,
            Inline(i, ..) => i,
            Integer(i, ..) => i,
            Let(i, ..) => i,
            NewOperator(i, ..) => i,
            OpenBrace(i, ..) => i,
            OpenBracket(i, ..) => i,
            OpenParen(i, ..) => i,
            Operator(i, ..) => i,
            Pragma(i, ..) => i,
            Pub(i, ..) => i,
            Return(i, ..) => i,
            SemiColon(i, ..) => i,
            Sizeof(i, ..) => i,
            Static(i, ..) => i,
            StringLiteral(i, ..) => i,
            Struct(i, ..) => i,
            Type(i, ..) => i,
            Variadic(i, ..) => i,
            While(i, ..) => i,
        }
    }
}
