use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    // Literals
    Identifier(Rc<str>),
    Integer(i128),
    Float(Rc<str>),
    StringLiteral(Rc<str>),
    Char(char),
    Operator(Rc<str>),

    // Delimiters: ( ) { } [ ] , ;
    OpenParen(),
    CloseParen(),
    OpenBrace(),
    CloseBrace(),
    OpenBracket(),
    CloseBracket(),
    Comma(),
    SemiColon(),

    // References: & &?
    Reference(),
    NullableReference(),

    // Keywords
    Pub(),
    Const(),
    Static(),
    Fun(),
    Let(),
    If(),
    Elif(),
    Else(),
    Type(),
    Struct(),
    Enum(),
    Return(),

    Include(),
    IncludeC(),
}

use Token::*;

pub static TOKEN_TYPES: &[(&str, fn() -> Token)] = &[
    (";", SemiColon),
    (",", Comma),
    ("(", OpenParen),
    (")", CloseParen),
    ("{", OpenBrace),
    ("}", CloseBrace),
    ("[", OpenBracket),
    ("]", CloseBracket),
    ("&?", NullableReference),
    ("&", Reference),
];

pub static KEYWORDS: &[(&str, fn() -> Token)] = &[
    ("pub", Pub),
    ("const", Const),
    ("static", Static),
    ("fun", Fun),
    ("let", Let),
    ("if", If),
    ("elif", Elif),
    ("else", Else),
    ("type", Type),
    ("struct", Struct),
    ("enum", Enum),
    ("return", Return),
];
