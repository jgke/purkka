use std::fmt;

use fragment::fragment::{FragmentIterator, Source};

use crate::tokentype::{Operator, Punctuation, OPERATORS, PUNCTUATION};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MacroTokenType {
    Identifier(String),
    Number(String),
    StringLiteral(String),
    Operator(Operator),
    Punctuation(Punctuation),
    Special(SpecialType),
    Other(char),
    Empty
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SpecialType {
    Asm(Vec<MacroToken>),
    Sizeof(SizeofExpression),
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MacroToken {
    pub source: Source,
    pub ty: MacroTokenType,
}

use ctoken::token::Token::*;
use ctoken::token::{Token, SizeofExpression};

pub fn preprocessor_to_parser(t: &MacroTokenType, index: usize) -> Token {
    match t {
        MacroTokenType::Operator(Operator::Dot) => Dot(index),
        MacroTokenType::Operator(Operator::Arrow) => Arrow(index),
        MacroTokenType::Operator(Operator::Increment) => Increment(index),
        MacroTokenType::Operator(Operator::Decrement) => Decrement(index),
        MacroTokenType::Operator(Operator::BitAnd) => BitAnd(index),
        MacroTokenType::Operator(Operator::Times) => Times(index),
        MacroTokenType::Operator(Operator::Plus) => Plus(index),
        MacroTokenType::Operator(Operator::Minus) => Minus(index),
        MacroTokenType::Operator(Operator::BitNot) => BitNot(index),
        MacroTokenType::Operator(Operator::Not) => Not(index),
        MacroTokenType::Operator(Operator::Divide) => Divide(index),
        MacroTokenType::Operator(Operator::Mod) => Mod(index),
        MacroTokenType::Operator(Operator::BitShiftLeft) => BitShiftLeft(index),
        MacroTokenType::Operator(Operator::BitShiftRight) => BitShiftRight(index),
        MacroTokenType::Operator(Operator::LessThan) => LessThan(index),
        MacroTokenType::Operator(Operator::MoreThan) => MoreThan(index),
        MacroTokenType::Operator(Operator::LessEqThan) => LessEqThan(index),
        MacroTokenType::Operator(Operator::MoreEqThan) => MoreEqThan(index),
        MacroTokenType::Operator(Operator::Equals) => Equals(index),
        MacroTokenType::Operator(Operator::NotEquals) => NotEquals(index),
        MacroTokenType::Operator(Operator::BitXor) => BitXor(index),
        MacroTokenType::Operator(Operator::BitOr) => BitOr(index),
        MacroTokenType::Operator(Operator::And) => And(index),
        MacroTokenType::Operator(Operator::Or) => Or(index),
        MacroTokenType::Operator(Operator::Terniary) => Terniary(index),
        MacroTokenType::Operator(Operator::TerniaryAlternative) => Colon(index),
        MacroTokenType::Operator(Operator::Assign) => Assign(index),
        MacroTokenType::Operator(Operator::TimesAssign) => TimesAssign(index),
        MacroTokenType::Operator(Operator::DivAssign) => DivAssign(index),
        MacroTokenType::Operator(Operator::ModAssign) => ModAssign(index),
        MacroTokenType::Operator(Operator::PlusAssign) => PlusAssign(index),
        MacroTokenType::Operator(Operator::MinusAssign) => MinusAssign(index),
        MacroTokenType::Operator(Operator::BitShiftLeftAssign) => BitShiftLeftAssign(index),
        MacroTokenType::Operator(Operator::BitShiftRightAssign) => BitShiftRightAssign(index),
        MacroTokenType::Operator(Operator::BitAndAssign) => BitAndAssign(index),
        MacroTokenType::Operator(Operator::BitXorAssign) => BitXorAssign(index),
        MacroTokenType::Operator(Operator::BitOrAssign) => BitOrAssign(index),
        MacroTokenType::Operator(Operator::Comma) => Comma(index),
        MacroTokenType::Operator(Operator::Macro) => Macro(index),
        MacroTokenType::Operator(Operator::MacroPaste) => MacroPaste(index),

        MacroTokenType::Identifier(ident) => {
            match ident.as_ref() {
                "auto" => Auto(index),
                "break" => Break(index),
                "case" => Case(index),
                "char" => Char(index),
                "const" => Const(index),
                "continue" => Continue(index),
                "default" => Default(index),
                "do" => Do(index),
                "double" => Double(index),
                "else" => Else(index),
                "enum" => Enum(index),
                "extern" => Extern(index),
                "float" => Float(index),
                "for" => For(index),
                "goto" => Goto(index),
                "if" => If(index),
                "int" => Int(index),
                "long" => Long(index),
                "register" => Register(index),
                "return" => Return(index),
                "short" => Short(index),
                "signed" => Signed(index),
                "sizeof" => panic!(), // these should be stripped out by now
                "static" => Static(index),
                "inline" => Inline(index),
                "struct" => Struct(index),
                "switch" => Switch(index),
                "typedef" => Typedef(index),
                "union" => Union(index),
                "unsigned" => Unsigned(index),
                "void" => Void(index),
                "volatile" => Volatile(index),
                "while" => While(index),
                s => Identifier(index, s.to_string()),
            }
        }

        // Punctuation
        MacroTokenType::Punctuation(Punctuation::OpenBracket) => OpenBracket(index),
        MacroTokenType::Punctuation(Punctuation::CloseBracket) => CloseBracket(index),
        MacroTokenType::Punctuation(Punctuation::OpenParen) => OpenParen(index),
        MacroTokenType::Punctuation(Punctuation::CloseParen) => CloseParen(index),
        MacroTokenType::Punctuation(Punctuation::OpenBrace) => OpenBrace(index),
        MacroTokenType::Punctuation(Punctuation::CloseBrace) => CloseBrace(index),
        MacroTokenType::Punctuation(Punctuation::Star) => Star(index),
        MacroTokenType::Punctuation(Punctuation::Comma) => Comma(index),
        MacroTokenType::Punctuation(Punctuation::Colon) => Colon(index),
        MacroTokenType::Punctuation(Punctuation::Assign) => Assign(index),
        MacroTokenType::Punctuation(Punctuation::Semicolon) => Semicolon(index),
        MacroTokenType::Punctuation(Punctuation::Varargs) => Varargs(index),

        MacroTokenType::StringLiteral(s) => StringLiteral(index, s.to_string()),
        MacroTokenType::Number(s) => Number(index, s.to_string()),
        MacroTokenType::Special(SpecialType::Sizeof(expr)) =>
            Sizeof(index, expr.clone()),
        MacroTokenType::Special(SpecialType::Asm(exprs)) =>
            Asm(index, exprs.iter().map(|t| t.to_src()).collect::<Vec<_>>().join(" ")),
        MacroTokenType::Other(_) => panic!(),
        MacroTokenType::Empty => panic!()
    }
}


impl MacroToken {
    pub fn display<'a>(&'a self, iter: &'a FragmentIterator) -> MacroTokenDisplay<'a> {
        MacroTokenDisplay { token: self, iter }
    }

    pub fn to_src(&self) -> String {
        match &self.ty {
            MacroTokenType::Operator(op) =>
                OPERATORS.iter().filter(|(_, t)| t == &op).map(|(s, _)| s).next().unwrap().to_string(),
            MacroTokenType::Punctuation(punc) =>
                PUNCTUATION.iter().filter(|(_, t)| t == &punc).map(|(s, _)| s).next().unwrap().to_string(),

            MacroTokenType::Identifier(ident) => ident.clone(),

            MacroTokenType::StringLiteral(s) => s.clone(),
            MacroTokenType::Number(s) => s.clone(),
            MacroTokenType::Special(SpecialType::Sizeof(expr)) =>
                format!("sizeof({:?})", expr),
            MacroTokenType::Special(SpecialType::Asm(exprs)) =>
                format!("asm ({})", exprs.iter().map(|t| t.to_src()).collect::<Vec<_>>().join(" ")),
            MacroTokenType::Other(c) => c.to_string(),
            MacroTokenType::Empty => " ".to_string()
        }
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
    pub(crate) fn dummy(ty: MacroTokenType) -> MacroToken {
        MacroToken {
            source: Source::dummy(),
            ty
        }
    }
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

    pub(crate) fn get_macro_paste_str(&self) -> Option<String> {
        match &self.ty {
            MacroTokenType::Identifier(ident) => Some(ident.clone()),
            MacroTokenType::Number(num) => Some(num.clone()),
            MacroTokenType::StringLiteral(s) => Some(format!("\"{}\"", s)),
            _ => None,
        }
    }
}

macro_rules! matches_token {
    ($var:expr, $type:ident, $subtype:ident) => {
        if let MacroTokenType::$type($type::$subtype) = &$var { true } else { false }
    }
}
