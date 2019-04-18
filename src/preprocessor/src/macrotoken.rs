use std::fmt;
use std::rc::Rc;

use fragment::fragment::{FragmentIterator, Source};

use crate::tokentype::{Operator, Punctuation, OPERATORS, PUNCTUATION};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MacroTokenType {
    Identifier(Rc<str>),
    Number(Rc<str>),
    StringLiteral(Rc<str>),
    Char(char),
    Operator(Operator),
    Punctuation(Punctuation),
    Special(SpecialType),
    Other(char),
    Empty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SpecialType {
    Asm(Vec<MacroToken>),
    Sizeof(Vec<MacroToken>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MacroToken {
    pub source: Source,
    pub ty: MacroTokenType,
}

use ctoken::token::Token;
use ctoken::token::Token::*;

pub fn preprocessor_to_parser(context: &FragmentIterator, t: &MacroToken, index: usize) -> Token {
    match &t.ty {
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
        MacroTokenType::Operator(Operator::Ternary) => Ternary(index),
        MacroTokenType::Operator(Operator::TernaryAlternative) => Colon(index),
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
        MacroTokenType::Operator(Operator::Macro) => panic!("Macro token found: {:?}", t),
        MacroTokenType::Operator(Operator::MacroPaste) => panic!("Macro token found: {:?}", t),

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
        MacroTokenType::Punctuation(Punctuation::Comma) => Comma(index),
        MacroTokenType::Punctuation(Punctuation::Colon) => Colon(index),
        MacroTokenType::Punctuation(Punctuation::Assign) => Assign(index),
        MacroTokenType::Punctuation(Punctuation::Semicolon) => Semicolon(index),
        MacroTokenType::Punctuation(Punctuation::Varargs) => Varargs(index),

        MacroTokenType::StringLiteral(s) => StringLiteral(index, s.to_string()),
        MacroTokenType::Number(s) => Number(index, s.to_string()),
        MacroTokenType::Special(SpecialType::Sizeof(expr)) => Sizeof(
            index,
            expr.iter()
                .map(|t| preprocessor_to_parser(context, t, index))
                .collect(),
        ),
        MacroTokenType::Special(SpecialType::Asm(exprs)) => Asm(
            index,
            exprs.iter()
                .map(|t| preprocessor_to_parser(context, t, index))
                .collect(),
        ),
        MacroTokenType::Char(c) => CharLiteral(index, *c),
        MacroTokenType::Other(c) => panic!(
            "Tried to convert Other({}) to parser\n{}",
            c,
            context.source_to_str(&t.source)
        ),
        MacroTokenType::Empty => panic!("Spurious empty token left in stream"),
    }
}

impl MacroToken {
    pub fn display<'a>(&'a self, iter: &'a FragmentIterator) -> MacroTokenDisplay<'a> {
        MacroTokenDisplay { token: self, iter }
    }

    pub fn to_src(&self) -> String {
        match &self.ty {
            MacroTokenType::Operator(op) => OPERATORS
                .iter()
                .filter(|(_, t)| t == &op)
                .map(|(s, _)| s)
                .next()
                .unwrap()
                .to_string(),
            MacroTokenType::Punctuation(punc) => PUNCTUATION
                .iter()
                .filter(|(_, t)| t == &punc)
                .map(|(s, _)| s)
                .next()
                .unwrap()
                .to_string(),

            MacroTokenType::Identifier(ident) => ident.to_string(),

            MacroTokenType::StringLiteral(s) => s.to_string(),
            MacroTokenType::Number(s) => s.to_string(),
            MacroTokenType::Special(SpecialType::Sizeof(expr)) => format!("sizeof({:?})", expr),
            MacroTokenType::Special(SpecialType::Asm(exprs)) => format!(
                "asm ({})",
                exprs
                    .iter()
                    .map(|t| t.to_src())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            MacroTokenType::Char(c) => format!("'{}'", c),
            MacroTokenType::Other(c) => c.to_string(),
            MacroTokenType::Empty => " ".to_string(),
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
            ty,
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

    pub(crate) fn get_identifier_str(&self) -> Option<Rc<str>> {
        match &self.ty {
            MacroTokenType::Identifier(ident) => Some(Rc::clone(ident)),
            _ => None,
        }
    }

    pub(crate) fn get_macro_paste_str(&self) -> Option<Rc<str>> {
        match &self.ty {
            MacroTokenType::Identifier(ident) => Some(Rc::clone(ident)),
            MacroTokenType::Number(num) => Some(Rc::clone(num)),
            MacroTokenType::StringLiteral(s) => Some(From::from(format!("\"{}\"", s))),
            _ => None,
        }
    }
}

macro_rules! matches_token {
    ($var:expr, $type:ident, $subtype:ident) => {
        if let MacroTokenType::$type($type::$subtype) = &$var {
            true
        } else {
            false
        }
    };
}
