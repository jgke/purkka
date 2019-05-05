pub mod parser;
pub mod visitor;

use std::rc::Rc;

use purkkasyntax::*;
use purkkatoken::tokenizer::tokenize;

use parser::parse;

pub fn parse_file(filename: &str, content: &str) -> S {
    let (tokens, _interner, fragment, sources) = tokenize(content, filename);
    parse(&mut tokens.iter().peekable(), &sources, &fragment)
}

pub fn get_declarations(tree: &S, include_private: bool) -> Vec<(Rc<str>, TypeSignature)> {
    let S::TranslationUnit(u) = tree;
    let TranslationUnit::Units(list) = u;
    list.iter()
        .flat_map(|unit|
             if let Unit::Declaration(decl) = unit {
                 match &**decl {
                     Declaration::Declaration(true, _, ident, Some(ty), _) =>
                         Some((ident.clone(), *ty.clone())),
                     Declaration::Declaration(false, _, ident, Some(ty), _) if include_private =>
                         Some((ident.clone(), *ty.clone())),
                     Declaration::Declaration(false, _, _, Some(_), _) => None,
                     Declaration::Declaration(_, _, _, None, _) => None,
                 }
             } else {
                 None
             })
        .collect()
}
