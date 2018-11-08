use tokentype::Operator::*;
use tokentype::Keyword::*;

#[derive(Debug)]
pub enum Operator {
    OpenBracket, CloseBracket,
    OpenParen, CloseParen,
    Dot, Arrow,
    Increment, Decrement, BitAnd, Times, Plus, Minus, BitNot, Not, SizeofOp,
    Divide, Mod, BitShiftLeft, BitShiftRight,
    LessThan, MoreThan, LessEqThan, MoreEqThan, Equals, NotEquals, BitXor, BitOr, And, Or,
    Terniary, TerniaryAlternative,
    Assign, TimesAssign, DivAssign, ModAssign, PlusAssign, MinusAssign,
    BitShiftLeftAssign, BitShiftRightAssign, BitAndAssign, BitXorAssign, BitOrAssign,
    Comma, Macro, MacroPaste
}

#[derive(Debug)]
pub enum Keyword {
    Auto, Break, Case, Char,
    Const, Continue, Default, Do,
    Double, Else, Enum, Extern,
    Float, For, Goto, If,
    Int, Long, Register, Return,
    Short, Signed, Sizeof, Static,
    Struct, Switch, Typedef, Union,
    Unsigned, Void, Volatile, While
}

#[derive(Debug)]
pub enum Punctuation {
    OpenBracket, CloseBracket,
    OpenParen, CloseParen,
    OpenBrace, CloseBrace,
    Star, Comma, Colon, Assign,
    Semicolon, Varargs, Macro
}

#[derive(Debug)]
pub enum Constant {
    Integer(String)
}

#[derive(Debug)]
pub enum TokenType {
    Keyword(&'static Keyword),
    Identifier(String),
    Constant(Constant),
    StringLiteral(String),
    Operator(&'static Operator),
    Punctuation(&'static Punctuation),
    Whitespace
}

pub static OPERATORS: &'static [(&'static str, &'static Operator)] = &[
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

    ("(", &OpenParen),
    (")", &CloseParen),
    ("[", &OpenBracket),
    ("]", &CloseBracket),

    ("?", &Terniary),
    (":", &TerniaryAlternative),

    (".", &Dot),
    (",", &Comma),

    ("#", &Macro),

    (">", &MoreThan),
    ("<", &LessThan),

    ("=", &Assign)
];

pub static PUNCTUATION: &'static [(&'static str, &'static Punctuation)] = &[
    ("[", &Punctuation::OpenBracket),
    ("]", &Punctuation::CloseBracket),
    ("(", &Punctuation::OpenParen),
    (")", &Punctuation::CloseParen),
    ("{", &Punctuation::OpenBrace),
    ("}", &Punctuation::CloseBrace),
    ("*", &Punctuation::Star),
    (",", &Punctuation::Comma),
    (":", &Punctuation::Colon),
    ("=", &Punctuation::Assign),
    (";", &Punctuation::Semicolon),
    ("...", &Punctuation::Varargs),
    ("#", &Punctuation::Macro)
] ;

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
    ("while", &While)
];
