#![feature(box_patterns)]

pub mod parser;
mod builtins;

use std::collections::HashSet;
use std::rc::Rc;

use purkkasyntax::*;
use purkkatoken::tokenizer::tokenize;
use resolve::{Declarations, FileQuery, ResolveResult};

use parser::{parse, Operators, Symbols};

pub fn parse_file(
    filename: &str,
    content: &str,
    get_file: &dyn Fn(FileQuery) -> ResolveResult,
    expand: &dyn Fn(String, HashSet<Rc<str>>) -> Vec<MacroExpansion>,
    expand_call: &dyn Fn(String, Vec<Expression>, HashSet<Rc<str>>) -> Vec<MacroExpansion>,
) -> (S, Operators, Symbols) {
    let (tokens, _interner, fragment, sources) = tokenize(content, filename);
    parse(
        tokens,
        &sources,
        &fragment,
        filename,
        get_file,
        expand,
        expand_call,
    )
}

pub fn get_declarations(tree: &S) -> (Declarations, Declarations)  {
    let S::TranslationUnit(u) = tree;
    let TranslationUnit::Units(list) = u;
    let (decls, types): (Vec<_>, Vec<_>) = list.iter()
        .map(|unit| {
            match unit {
                Unit::Declaration(box Declaration::Declaration(_, ident, ty, _)) => {
                    (Some((ident.clone(), *ty.clone())), None)
                }
                // XXX: fix
                _ => (None, None)
                //t => unimplemented!("{:?}", t)
            }
        })
        .unzip();
    (decls.into_iter().flatten().collect(), types.iter().cloned().flatten().collect())
}
