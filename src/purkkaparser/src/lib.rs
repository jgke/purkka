pub mod parser;

use std::collections::HashSet;
use std::rc::Rc;

use purkkasyntax::*;
use purkkatoken::tokenizer::tokenize;
use resolve::{FileQuery, ResolveResult};

use parser::{parse, Operators, Symbols};

pub fn parse_file(
    filename: &str,
    content: &str,
    get_file: &dyn Fn(FileQuery) -> ResolveResult,
    expand: &dyn Fn(String, HashSet<Rc<str>>) -> Vec<MacroExpansion>,
) -> (S, Operators, Symbols) {
    let (tokens, _interner, fragment, sources) = tokenize(content, filename);
    parse(
        &mut tokens.iter().peekable(),
        &sources,
        &fragment,
        filename,
        get_file,
        expand
    )
}

pub fn get_declarations(tree: &S, include_private: bool) -> Vec<(Rc<str>, TypeSignature)> {
    let S::TranslationUnit(u) = tree;
    let TranslationUnit::Units(list) = u;
    list.iter()
        .flat_map(|unit| {
            if let Unit::Declaration(decl) = unit {
                match &**decl {
                    Declaration::Declaration(true, _, _, ident, ty, _) => {
                        Some((ident.clone(), *ty.clone()))
                    }
                    Declaration::Declaration(false, _, _, ident, ty, _) if include_private => {
                        Some((ident.clone(), *ty.clone()))
                    }
                    Declaration::Declaration(false, _, _, _, _, _) => None,
                }
            } else {
                None
            }
        })
        .collect()
}
