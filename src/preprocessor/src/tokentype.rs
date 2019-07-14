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
pub enum Punctuation {
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    Semicolon,
    Varargs,
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

pub static PUNCTUATION: &[(&str, &Punctuation)] = &[
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

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match &self {
            Operator::BitShiftLeftAssign => "<<=",
            Operator::BitShiftRightAssign => ">>=",
            Operator::ModAssign => "%=",
            Operator::BitAndAssign => "&=",
            Operator::TimesAssign => "*=",
            Operator::PlusAssign => "+=",
            Operator::MinusAssign => "-=",
            Operator::DivAssign => "/=",
            Operator::BitXorAssign => "^=",
            Operator::BitOrAssign => "|=",
            Operator::Increment => "++",
            Operator::Decrement => "--",
            Operator::BitShiftLeft => "<<",
            Operator::BitShiftRight => ">>",
            Operator::Arrow => "->",
            Operator::NotEquals => "!=",
            Operator::And => "&&",
            Operator::LessEqThan => "<=",
            Operator::Equals => "==",
            Operator::MoreEqThan => ">=",
            Operator::Or => "||",
            Operator::MacroPaste => "##",
            Operator::Plus => "+",
            Operator::Mod => "%",
            Operator::BitAnd => "&",
            Operator::Times => "*",
            Operator::Divide => "/",
            Operator::Minus => "-",
            Operator::BitOr => "|",
            Operator::BitXor => "^",
            Operator::BitNot => "~",
            Operator::Not => "!",
            Operator::Ternary => "?",
            Operator::TernaryAlternative => ":",
            Operator::Dot => ".",
            Operator::Comma => ",",
            Operator::Macro => "#",
            Operator::MoreThan => ">",
            Operator::LessThan => "<",
            Operator::Assign => "=",
        };
        write!(f, "{}", s)
    }
}

impl std::fmt::Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match &self {
            Punctuation::OpenBracket => "[",
            Punctuation::CloseBracket => "]",
            Punctuation::OpenParen => "(",
            Punctuation::CloseParen => ")",
            Punctuation::OpenBrace => "{",
            Punctuation::CloseBrace => "}",
            Punctuation::Comma => ",",
            Punctuation::Semicolon => ";",
            Punctuation::Varargs => "...",
        };
        write!(f, "{}", s)
    }
}
