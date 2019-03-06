#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    // Operator
    Dot(),
    Arrow(),
    Increment(),
    Decrement(),
    BitAnd(),
    Times(),
    Plus(),
    Minus(),
    BitNot(),
    Not(),
    Divide(),
    Mod(),
    BitShiftLeft(),
    BitShiftRight(),
    LessThan(),
    MoreThan(),
    LessEqThan(),
    MoreEqThan(),
    Equals(),
    NotEquals(),
    BitXor(),
    BitOr(),
    And(),
    Or(),
    Terniary(),
    Assign(),
    TimesAssign(),
    DivAssign(),
    ModAssign(),
    PlusAssign(),
    MinusAssign(),
    BitShiftLeftAssign(),
    BitShiftRightAssign(),
    BitAndAssign(),
    BitXorAssign(),
    BitOrAssign(),
    Comma(),
    Macro(),
    MacroPaste(),

    // Keyword
    Auto(),
    Break(),
    Case(),
    Char(),
    Const(),
    Continue(),
    Default(),
    Do(),
    Double(),
    Else(),
    Enum(),
    Extern(),
    Float(),
    For(),
    Goto(),
    If(),
    Int(),
    Long(),
    Register(),
    Return(),
    Short(),
    Signed(),
    Sizeof(),
    Static(),
    Struct(),
    Switch(),
    Typedef(),
    Union(),
    Unsigned(),
    Void(),
    Volatile(),
    While(),

    // Punctuation
    OpenBracket(),
    CloseBracket(),
    OpenParen(),
    CloseParen(),
    OpenBrace(),
    CloseBrace(),
    Star(),
    Colon(),
    Semicolon(),
    Varargs(),

    Identifier(String),
    StringLiteral(),
    Number(),
}