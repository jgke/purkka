use tokenizer::Operator::*;

#[derive(Debug)]
pub enum Operator {
    OpenBracket, CloseBracket,
    OpenParen, CloseParen,
    Dot, Arrow,
    Increment, Decrement, BitAnd, Times, Plus, Minus, BitNot, Not, Sizeof,
    Divide, Mod, BitShiftLeft, BitShiftRight,
    LessThan, MoreThan, LessEqThan, MoreEqThan, Equals, NotEquals, BitXor, BitOr, And, Or,
    Terniary, TerniaryAlternative,
    Assign, TimesAssign, DivAssign, ModAssign, PlusAssign, MinusAssign,
    BitShiftLeftAssign, BitShiftRightAssign, BitAndAssign, BitXorAssign, BitOrAssign,
    Comma, Macro, MacroPaste
}

#[derive(Debug)]
pub enum Punctuation {
}

#[derive(Debug)]
pub enum Constant {
    Integer(String)
}

#[derive(Debug)]
pub enum TokenType {
    Keyword(String),
    Identifier(String),
    Constant(Constant),
    StringLiteral(String),
    Operator(&'static Operator),
    Punctuator(String),
    Whitespace()
}

pub fn read_number(s: String) -> (TokenType, String) {
    let mut number = String::from("");
    let mut s_iter = s.chars().peekable();

    while let Some(c) = s_iter.peek().cloned() {
        match c {
            '0' ..= '9' => {
                number.push(c);
                s_iter.next();
            },
            _ => break
        }

    }

    let len = number.len();
    unsafe {
        (TokenType::Constant(Constant::Integer(number)), s.get_unchecked(len..).to_string())
    }
}

pub fn read_identifier(s: String) -> (TokenType, String) {
    let mut identifier = String::from("");
    let mut s_iter = s.chars().peekable();

    while let Some(c) = s_iter.peek().cloned() {
        match c {
            '0' ..= '9' | 'a' ..= 'z' | 'A' ..= 'Z' => {
                identifier.push(c);
                s_iter.next();
            }
            _ => break
        };
    }

    let len = identifier.len();
    unsafe {
        (TokenType::Identifier(identifier), s.get_unchecked(len..).to_string())
    }
}

pub fn read_whitespace(s: String) -> (TokenType, String) {
    let mut len = 0;
    let mut s_iter = s.chars().peekable();

    while let Some(c) = s_iter.peek().cloned() {
        match c {
            ' ' | '\t' | '\n' => {
                s_iter.next();
                len += 1;
            }
            _ => break
        };
    }

    unsafe {
        (TokenType::Whitespace(), s.get_unchecked(len..).to_string())
    }
}

static OPERATORS: &'static [(&'static str, &'static Operator)] = &[
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

fn match_operator(s: String) -> Option<(TokenType, String)> {
    for (operator, op) in OPERATORS.iter() {
        if s.starts_with(operator) {
            unsafe {
                return Some((TokenType::Operator(op), s.get_unchecked(operator.len()..).to_string()));
            }
        }
    }
    None
}

pub fn read_token(s: String) -> Option<(TokenType, String)> {
    match s.chars().next() {
        Some(c) => {
            match c {
                '0' ..= '9' => Some(read_number(s)),
                'a' ..= 'z' | 'A' ..= 'Z' => Some(read_identifier(s)),
                ' ' | '\t' | '\n' => Some(read_whitespace(s)),
                _ => match_operator(s)
            }
        }
        None => None
    }
}
