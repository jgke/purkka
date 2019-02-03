use preprocessor::macrotoken::{MacroTokenType};
use preprocessor::tokentype::{Operator, Punctuation};

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

    Identifier(),
    StringLiteral(),
    Number(),
}

use token::Token::*;

fn preprocessor_to_parser(t: &MacroTokenType) -> Token {
    match t {
        MacroTokenType::Operator(Operator::Dot) => Dot(),
        MacroTokenType::Operator(Operator::Arrow) => Arrow(),
        MacroTokenType::Operator(Operator::Increment) => Increment(),
        MacroTokenType::Operator(Operator::Decrement) => Decrement(),
        MacroTokenType::Operator(Operator::BitAnd) => BitAnd(),
        MacroTokenType::Operator(Operator::Times) => Times(),
        MacroTokenType::Operator(Operator::Plus) => Plus(),
        MacroTokenType::Operator(Operator::Minus) => Minus(),
        MacroTokenType::Operator(Operator::BitNot) => BitNot(),
        MacroTokenType::Operator(Operator::Not) => Not(),
        MacroTokenType::Operator(Operator::Divide) => Divide(),
        MacroTokenType::Operator(Operator::Mod) => Mod(),
        MacroTokenType::Operator(Operator::BitShiftLeft) => BitShiftLeft(),
        MacroTokenType::Operator(Operator::BitShiftRight) => BitShiftRight(),
        MacroTokenType::Operator(Operator::LessThan) => LessThan(),
        MacroTokenType::Operator(Operator::MoreThan) => MoreThan(),
        MacroTokenType::Operator(Operator::LessEqThan) => LessEqThan(),
        MacroTokenType::Operator(Operator::MoreEqThan) => MoreEqThan(),
        MacroTokenType::Operator(Operator::Equals) => Equals(),
        MacroTokenType::Operator(Operator::NotEquals) => NotEquals(),
        MacroTokenType::Operator(Operator::BitXor) => BitXor(),
        MacroTokenType::Operator(Operator::BitOr) => BitOr(),
        MacroTokenType::Operator(Operator::And) => And(),
        MacroTokenType::Operator(Operator::Or) => Or(),
        MacroTokenType::Operator(Operator::Terniary) => Terniary(),
        MacroTokenType::Operator(Operator::TerniaryAlternative) => Colon(),
        MacroTokenType::Operator(Operator::Assign) => Assign(),
        MacroTokenType::Operator(Operator::TimesAssign) => TimesAssign(),
        MacroTokenType::Operator(Operator::DivAssign) => DivAssign(),
        MacroTokenType::Operator(Operator::ModAssign) => ModAssign(),
        MacroTokenType::Operator(Operator::PlusAssign) => PlusAssign(),
        MacroTokenType::Operator(Operator::MinusAssign) => MinusAssign(),
        MacroTokenType::Operator(Operator::BitShiftLeftAssign) => BitShiftLeftAssign(),
        MacroTokenType::Operator(Operator::BitShiftRightAssign) => BitShiftRightAssign(),
        MacroTokenType::Operator(Operator::BitAndAssign) => BitAndAssign(),
        MacroTokenType::Operator(Operator::BitXorAssign) => BitXorAssign(),
        MacroTokenType::Operator(Operator::BitOrAssign) => BitOrAssign(),
        MacroTokenType::Operator(Operator::Comma) => Comma(),
        MacroTokenType::Operator(Operator::Macro) => Macro(),
        MacroTokenType::Operator(Operator::MacroPaste) => MacroPaste(),

        MacroTokenType::Identifier(ident) => {
            match ident.as_ref() {
                "auto" => Auto(),
                "break" => Break(),
                "case" => Case(),
                "char" => Char(),
                "const" => Const(),
                "continue" => Continue(),
                "default" => Default(),
                "do" => Do(),
                "double" => Double(),
                "else" => Else(),
                "enum" => Enum(),
                "extern" => Extern(),
                "float" => Float(),
                "for" => For(),
                "goto" => Goto(),
                "if" => If(),
                "int" => Int(),
                "long" => Long(),
                "register" => Register(),
                "return" => Return(),
                "short" => Short(),
                "signed" => Signed(),
                "sizeof" => Sizeof(),
                "static" => Static(),
                "struct" => Struct(),
                "switch" => Switch(),
                "typedef" => Typedef(),
                "union" => Union(),
                "unsigned" => Unsigned(),
                "void" => Void(),
                "volatile" => Volatile(),
                "while" => While(),
                _ => Identifier(),
            }
        }

        // Punctuation
        MacroTokenType::Punctuation(Punctuation::OpenBracket) => OpenBracket(),
        MacroTokenType::Punctuation(Punctuation::CloseBracket) => CloseBracket(),
        MacroTokenType::Punctuation(Punctuation::OpenParen) => OpenParen(),
        MacroTokenType::Punctuation(Punctuation::CloseParen) => CloseParen(),
        MacroTokenType::Punctuation(Punctuation::OpenBrace) => OpenBrace(),
        MacroTokenType::Punctuation(Punctuation::CloseBrace) => CloseBrace(),
        MacroTokenType::Punctuation(Punctuation::Star) => Star(),
        MacroTokenType::Punctuation(Punctuation::Comma) => Comma(),
        MacroTokenType::Punctuation(Punctuation::Colon) => Colon(),
        MacroTokenType::Punctuation(Punctuation::Assign) => Assign(),
        MacroTokenType::Punctuation(Punctuation::Semicolon) => Semicolon(),
        MacroTokenType::Punctuation(Punctuation::Varargs) => Varargs(),

        MacroTokenType::StringLiteral(_) => StringLiteral(),
        MacroTokenType::Number(_) => Number(),
        MacroTokenType::Other(_) => panic!()
    }
}
