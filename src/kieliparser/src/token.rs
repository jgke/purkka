use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(Rc<str>),
    Integer(i128),
    Float(Rc<str>),
    StringLiteral(Rc<str>),
    Char(char),
    Operator(Rc<str>),

    OpenParen(),
    Comma(),
    CloseParen(),

    Colon(),
    SemiColon(),

    Pub(),
    Const(),
    Static(),
    Fun(),
    Let(),

    Include(),
    IncludeC(),
}

use Token::*;

pub static TOKEN_TYPES: &[(&str, fn() -> Token)] = &[
    (";", SemiColon),
    ("(", OpenParen),
    (")", CloseParen),
];

pub static KEYWORDS: &[(&str, fn() -> Token)] = &[
    ("pub", Pub),
    ("const", Const),
    ("static", Static),
    ("fun", Fun),
    ("let", Let),
];
