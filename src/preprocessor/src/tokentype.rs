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
    Terniary,
    TerniaryAlternative,
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
    Star,
    Comma,
    Colon,
    Assign,
    Semicolon,
    Varargs,
    //Macro
}

//#[derive(Clone, Debug)]
//pub enum Macro {
//    MacroIf,
//    MacroIfdef,
//    MacroIfndef,
//    MacroElif,
//    MacroEndif,
//    Define, Undef,
//    Line, Error, Pragma
//}

//#[derive(Clone, Debug)]
//pub enum MacroInclude {
//    IncludeSystem(String),
//    IncludeLocal(String),
//}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Constant {
    Integer(),
}

pub type Identifier = String;
pub type StringLiteral = String;

//#[derive(Clone, Copy, Debug, PartialEq, Eq)]
//pub enum TokenType {
//    Keyword(&'static Keyword),
//    Identifier(),
//    Constant(Constant),
//    StringLiteral(StringLiteral),
//    Operator(&'static Operator),
//    UnaryOperator(UnaryOperator),
//    Punctuation(&'static Punctuation),
//    //Macro(&'static Macro),
//    //MacroInclude(MacroInclude),
//    Whitespace
//}

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
    ("?", &Terniary),
    (":", &TerniaryAlternative),
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

    (12, &Terniary),
    (12, &TerniaryAlternative),
];

// return (precedence, is-right-to-left)
pub fn get_precedence(op: &Operator) -> usize {
    for (precedence, ty) in PRECEDENCE {
        if *ty == op {
            return *precedence;
        }
    }
    panic!();
}

pub fn is_left_associative(op: &Operator) -> bool {
    match op {
        Terniary | TerniaryAlternative => false,
        _ => true
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

//pub static SIMPLE_MACROS: &'static [(&'static str, &'static Macro)] = &[
//    ("if ", &MacroIf),
//    ("ifdef ", &MacroIfdef),
//    ("ifndef ", &MacroIfndef),
//    ("elif ", &MacroElif),
//    ("endif ", &MacroEndif),
//    ("define ", &Define),
//    ("undef ", &Undef),
//    ("line ", &Line),
//    ("error ", &Error),
//    ("pragma ", &Pragma)
//];
