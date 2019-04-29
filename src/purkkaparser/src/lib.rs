pub mod parser;
pub mod visitor;

use purkkasyntax::S;
use purkkatoken::tokenizer::tokenize;

use parser::parse;

pub fn parse_file(filename: &str, content: &str) -> S {
    let (tokens, _interner, fragment, sources) = tokenize(content, filename);
    parse(&mut tokens.iter().peekable(), &sources, &fragment)
}
