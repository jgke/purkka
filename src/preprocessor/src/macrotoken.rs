use std::fmt;

use shared::fragment::{FragmentIterator, Source};

use tokentype::{Operator, Punctuation};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MacroTokenType {
    Identifier(String),
    Number(String),
    StringLiteral(String),
    Operator(Operator),
    Punctuation(Punctuation),
    Other(char),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MacroToken {
    pub source: Source,
    pub ty: MacroTokenType,
}

use ctoken::token::Token::*;
use ctoken::token::Token;

pub fn preprocessor_to_parser(t: &MacroTokenType) -> Token {
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


impl MacroToken {
    pub fn display<'a>(&'a self, iter: &'a FragmentIterator) -> MacroTokenDisplay<'a> {
        MacroTokenDisplay { token: self, iter }
    }
}

pub struct MacroTokenDisplay<'a> {
    token: &'a MacroToken,
    iter: &'a FragmentIterator,
}

impl fmt::Debug for MacroTokenDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for MacroTokenDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "type: {:?}\nsource:\n{}",
            self.token.ty,
            self.iter.source_to_str(&self.token.source)
        )
    }
}

impl MacroToken {
    pub(crate) fn respan_front(&mut self, source: &Source) {
        let old_source = self.source.clone();
        let mut new_source = source.clone();
        new_source.merge(&old_source);
        self.source = new_source;
    }
    pub(crate) fn respan_back(&mut self, source: &Source) {
        self.source.merge(source)
    }

    pub(crate) fn get_identifier_str(&self) -> Option<String> {
        match &self.ty {
            MacroTokenType::Identifier(ident) => Some(ident.clone()),
            _ => None,
        }
    }
}
