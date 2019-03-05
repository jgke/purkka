extern crate preprocessor;
extern crate cparser;
extern crate ctoken;

use ctoken::token::Token;
use preprocessor::macrotoken::{MacroToken, preprocessor_to_parser};

pub fn parse(input: Vec<MacroToken>) -> Option<cparser::parser::S> {
    let mut tokens: Vec<Token> = input.iter().map(|t| preprocessor_to_parser(&t.ty)).collect();
    cparser::parse(tokens)
}
