use std::rc::Rc;

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    // Operator
    Dot(usize),
    Arrow(usize),
    Increment(usize),
    Decrement(usize),
    BitAnd(usize),
    Times(usize),
    Plus(usize),
    Minus(usize),
    BitNot(usize),
    Not(usize),
    Divide(usize),
    Mod(usize),
    BitShiftLeft(usize),
    BitShiftRight(usize),
    LessThan(usize),
    MoreThan(usize),
    LessEqThan(usize),
    MoreEqThan(usize),
    Equals(usize),
    NotEquals(usize),
    BitXor(usize),
    BitOr(usize),
    And(usize),
    Or(usize),
    Ternary(usize),
    Assign(usize),
    TimesAssign(usize),
    DivAssign(usize),
    ModAssign(usize),
    PlusAssign(usize),
    MinusAssign(usize),
    BitShiftLeftAssign(usize),
    BitShiftRightAssign(usize),
    BitAndAssign(usize),
    BitXorAssign(usize),
    BitOrAssign(usize),
    Comma(usize),
    Macro(usize),
    MacroPaste(usize),

    // Keyword
    Asm(usize),
    Auto(usize),
    Break(usize),
    Case(usize),
    Char(usize),
    Const(usize),
    Continue(usize),
    Default(usize),
    Do(usize),
    Double(usize),
    Else(usize),
    Enum(usize),
    Extern(usize),
    Float(usize),
    For(usize),
    Goto(usize),
    If(usize),
    Int(usize),
    Long(usize),
    Register(usize),
    Return(usize),
    Short(usize),
    Signed(usize),
    Sizeof(usize),
    Static(usize),
    Inline(usize),
    Struct(usize),
    Switch(usize),
    Typedef(usize),
    Union(usize),
    Unsigned(usize),
    Void(usize),
    Volatile(usize),
    While(usize),

    // Punctuation
    OpenBracket(usize),
    CloseBracket(usize),
    OpenParen(usize),
    CloseParen(usize),
    OpenBrace(usize),
    CloseBrace(usize),
    Colon(usize),
    Semicolon(usize),
    Varargs(usize),

    // Literals
    Identifier(usize, Rc<str>),
    StringLiteral(usize, Rc<str>),
    CharLiteral(usize, char),
    Number(usize, Rc<str>),
}

impl Token {
    pub fn get_num(&self) -> usize {
        use Token::*;
        *match self {
            // Operator
            Dot(i) => i,
            Arrow(i) => i,
            Increment(i) => i,
            Decrement(i) => i,
            BitAnd(i) => i,
            Times(i) => i,
            Plus(i) => i,
            Minus(i) => i,
            BitNot(i) => i,
            Not(i) => i,
            Divide(i) => i,
            Mod(i) => i,
            BitShiftLeft(i) => i,
            BitShiftRight(i) => i,
            LessThan(i) => i,
            MoreThan(i) => i,
            LessEqThan(i) => i,
            MoreEqThan(i) => i,
            Equals(i) => i,
            NotEquals(i) => i,
            BitXor(i) => i,
            BitOr(i) => i,
            And(i) => i,
            Or(i) => i,
            Ternary(i) => i,
            Assign(i) => i,
            TimesAssign(i) => i,
            DivAssign(i) => i,
            ModAssign(i) => i,
            PlusAssign(i) => i,
            MinusAssign(i) => i,
            BitShiftLeftAssign(i) => i,
            BitShiftRightAssign(i) => i,
            BitAndAssign(i) => i,
            BitXorAssign(i) => i,
            BitOrAssign(i) => i,
            Comma(i) => i,
            Macro(i) => i,
            MacroPaste(i) => i,

            // Keyword
            Auto(i) => i,
            Break(i) => i,
            Case(i) => i,
            Char(i) => i,
            Const(i) => i,
            Continue(i) => i,
            Default(i) => i,
            Do(i) => i,
            Double(i) => i,
            Else(i) => i,
            Enum(i) => i,
            Extern(i) => i,
            Float(i) => i,
            For(i) => i,
            Goto(i) => i,
            If(i) => i,
            Int(i) => i,
            Long(i) => i,
            Register(i) => i,
            Return(i) => i,
            Short(i) => i,
            Signed(i) => i,
            Static(i) => i,
            Inline(i) => i,
            Struct(i) => i,
            Switch(i) => i,
            Typedef(i) => i,
            Union(i) => i,
            Unsigned(i) => i,
            Void(i) => i,
            Volatile(i) => i,
            While(i) => i,

            // Punctuation
            OpenBracket(i) => i,
            CloseBracket(i) => i,
            OpenParen(i) => i,
            CloseParen(i) => i,
            OpenBrace(i) => i,
            CloseBrace(i) => i,
            Colon(i) => i,
            Semicolon(i) => i,
            Varargs(i) => i,

            // Special forms
            Sizeof(i, ..) => i,
            Asm(i, ..) => i,

            // Literals
            Identifier(i, ..) => i,
            StringLiteral(i, ..) => i,
            CharLiteral(i, ..) => i,
            Number(i, ..) => i,
        }
    }

    pub fn get_ident_str(&self) -> &Rc<str> {
        match self {
            Token::Identifier(_, i) => i,
            Token::Number(_, i) => i,
            Token::StringLiteral(_, i) => i,
            _ => panic!(),
        }
    }

    pub fn get_char_val(&self) -> char {
        if let Token::CharLiteral(_, i) = self {
            *i
        } else {
            panic!()
        }
    }
}
