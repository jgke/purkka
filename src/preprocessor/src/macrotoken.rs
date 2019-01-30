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
