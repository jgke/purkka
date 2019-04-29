use crate::tokentype::Keyword::*;
use crate::tokentype::Operator::*;

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Dot,
    Arrow,
    Increment,
    Decrement,
    BitAnd,
    Times,
    Plus,
    Minus,
    BitNot,
    Not, /* SizeofOp, */
    Divide,
    Mod,
    BitShiftLeft,
    BitShiftRight,
    LessThan,
    MoreThan,
    LessEqThan,
    MoreEqThan,
    Equals,
    NotEquals,
    BitXor,
    BitOr,
    And,
    Or,
    Ternary,
    TernaryAlternative,
    Assign,
    TimesAssign,
    DivAssign,
    ModAssign,
    PlusAssign,
    MinusAssign,
    BitShiftLeftAssign,
    BitShiftRightAssign,
    BitAndAssign,
    BitXorAssign,
    BitOrAssign,
    Comma,
    Macro,
    MacroPaste,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    BitNot,
    Not,
    Dereference,
    AddressOf,
    UnaryPlus,
    UnaryMinus,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Int,
    Long,
    Register,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StorageClass {
    Auto,
    Extern,
    Register,
    Static,
    Typedef,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeSpecifier {
    Char,
    Double,
    Float,
    Int,
    Long,
    Short,
    Void,
    Enum,
    Struct,
    Union,
    Signed,
    Unsigned,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Punctuation {
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    Colon,
    Assign,
    Semicolon,
    Varargs,
    //Macro
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Constant {
    Integer(),
}

pub static OPERATORS: &[(&str, &Operator)] = &[
    ("<<=", &BitShiftLeftAssign),
    (">>=", &BitShiftRightAssign),
    ("%=", &ModAssign),
    ("&=", &BitAndAssign),
    ("*=", &TimesAssign),
    ("+=", &PlusAssign),
    ("-=", &MinusAssign),
    ("/=", &DivAssign),
    ("^=", &BitXorAssign),
    ("|=", &BitOrAssign),
    ("++", &Increment),
    ("--", &Decrement),
    ("<<", &BitShiftLeft),
    (">>", &BitShiftRight),
    ("->", &Arrow),
    ("!=", &NotEquals),
    ("&&", &And),
    ("<=", &LessEqThan),
    ("==", &Equals),
    (">=", &MoreEqThan),
    ("||", &Or),
    ("##", &MacroPaste),
    ("+", &Plus),
    ("%", &Mod),
    ("&", &BitAnd),
    ("*", &Times),
    ("/", &Divide),
    ("-", &Minus),
    ("|", &BitOr),
    ("^", &BitXor),
    ("~", &BitNot),
    ("!", &Not),
    ("?", &Ternary),
    (":", &TernaryAlternative),
    (".", &Dot),
    (",", &Comma),
    ("#", &Macro),
    (">", &MoreThan),
    ("<", &LessThan),
    ("=", &Assign),
];

// constant expression operator precedence
pub static PRECEDENCE: &[(usize, &Operator)] = &[
    (1, &Not),
    (1, &BitNot),
    (2, &Mod),
    (2, &Times),
    (2, &Divide),
    (3, &Plus),
    (3, &Minus),
    (4, &BitShiftLeft),
    (4, &BitShiftRight),
    (5, &MoreThan),
    (5, &LessThan),
    (5, &LessEqThan),
    (5, &MoreEqThan),
    (6, &NotEquals),
    (6, &Equals),
    (7, &BitAnd),
    (8, &BitXor),
    (9, &BitOr),
    (10, &And),
    (11, &Or),
    (12, &Ternary),
    (12, &TernaryAlternative),
];

// return (precedence, is-right-to-left)
pub fn get_precedence(op: Operator) -> usize {
    for (precedence, ty) in PRECEDENCE {
        if **ty == op {
            return *precedence;
        }
    }
    panic!();
}

pub fn is_left_associative(op: Operator) -> bool {
    match op {
        Ternary | TernaryAlternative => false,
        _ => true,
    }
}

pub static PUNCTUATION: &'static [(&'static str, &'static Punctuation)] = &[
    ("[", &Punctuation::OpenBracket),
    ("]", &Punctuation::CloseBracket),
    ("(", &Punctuation::OpenParen),
    (")", &Punctuation::CloseParen),
    ("{", &Punctuation::OpenBrace),
    ("}", &Punctuation::CloseBrace),
    (",", &Punctuation::Comma),
    (";", &Punctuation::Semicolon),
    ("...", &Punctuation::Varargs),
    //("#", &Punctuation::Macro)
];

pub static KEYWORDS: &'static [(&'static str, &'static Keyword)] = &[
    ("auto", &Auto),
    ("break", &Break),
    ("case", &Case),
    ("char", &Char),
    ("const", &Const),
    ("continue", &Continue),
    ("default", &Default),
    ("do", &Do),
    ("double", &Double),
    ("else", &Else),
    ("enum", &Enum),
    ("extern", &Extern),
    ("float", &Float),
    ("for", &For),
    ("goto", &Goto),
    ("if", &If),
    ("int", &Int),
    ("long", &Long),
    ("register", &Register),
    ("return", &Return),
    ("short", &Short),
    ("signed", &Signed),
    ("sizeof", &Sizeof),
    ("static", &Static),
    ("struct", &Struct),
    ("switch", &Switch),
    ("typedef", &Typedef),
    ("union", &Union),
    ("unsigned", &Unsigned),
    ("void", &Void),
    ("volatile", &Volatile),
    ("while", &While),
];
