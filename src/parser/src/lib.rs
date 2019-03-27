extern crate preprocessor;
extern crate cparser;
extern crate ctoken;
extern crate shared;

use ctoken::token::Token;
use preprocessor::macrotoken::{MacroToken, preprocessor_to_parser};
use shared::fragment::{FragmentIterator, Source};

fn get_source_index(token: &Token) -> usize {
    match token {
        Token::Dot(index,) => *index,
        Token::Arrow(index,) => *index,
        Token::Increment(index,) => *index,
        Token::Decrement(index,) => *index,
        Token::BitAnd(index,) => *index,
        Token::Times(index,) => *index,
        Token::Plus(index,) => *index,
        Token::Minus(index,) => *index,
        Token::BitNot(index,) => *index,
        Token::Not(index,) => *index,
        Token::Divide(index,) => *index,
        Token::Mod(index,) => *index,
        Token::BitShiftLeft(index,) => *index,
        Token::BitShiftRight(index,) => *index,
        Token::LessThan(index,) => *index,
        Token::MoreThan(index,) => *index,
        Token::LessEqThan(index,) => *index,
        Token::MoreEqThan(index,) => *index,
        Token::Equals(index,) => *index,
        Token::NotEquals(index,) => *index,
        Token::BitXor(index,) => *index,
        Token::BitOr(index,) => *index,
        Token::And(index,) => *index,
        Token::Or(index,) => *index,
        Token::Terniary(index,) => *index,
        Token::Assign(index,) => *index,
        Token::TimesAssign(index,) => *index,
        Token::DivAssign(index,) => *index,
        Token::ModAssign(index,) => *index,
        Token::PlusAssign(index,) => *index,
        Token::MinusAssign(index,) => *index,
        Token::BitShiftLeftAssign(index,) => *index,
        Token::BitShiftRightAssign(index,) => *index,
        Token::BitAndAssign(index,) => *index,
        Token::BitXorAssign(index,) => *index,
        Token::BitOrAssign(index,) => *index,
        Token::Comma(index,) => *index,
        Token::Macro(index,) => *index,
        Token::MacroPaste(index,) => *index,
        Token::Auto(index,) => *index,
        Token::Break(index,) => *index,
        Token::Case(index,) => *index,
        Token::Char(index,) => *index,
        Token::Const(index,) => *index,
        Token::Continue(index,) => *index,
        Token::Default(index,) => *index,
        Token::Do(index,) => *index,
        Token::Double(index,) => *index,
        Token::Else(index,) => *index,
        Token::Enum(index,) => *index,
        Token::Extern(index,) => *index,
        Token::Float(index,) => *index,
        Token::For(index,) => *index,
        Token::Goto(index,) => *index,
        Token::If(index,) => *index,
        Token::Int(index,) => *index,
        Token::Inline(index,) => *index,
        Token::Long(index,) => *index,
        Token::Register(index,) => *index,
        Token::Return(index,) => *index,
        Token::Short(index,) => *index,
        Token::Signed(index,) => *index,
        Token::Sizeof(index,_) => *index,
        Token::Static(index,) => *index,
        Token::Struct(index,) => *index,
        Token::Switch(index,) => *index,
        Token::Typedef(index,) => *index,
        Token::Union(index,) => *index,
        Token::Unsigned(index,) => *index,
        Token::Void(index,) => *index,
        Token::Volatile(index,) => *index,
        Token::While(index,) => *index,
        Token::OpenBracket(index,) => *index,
        Token::CloseBracket(index,) => *index,
        Token::OpenParen(index,) => *index,
        Token::CloseParen(index,) => *index,
        Token::OpenBrace(index,) => *index,
        Token::CloseBrace(index,) => *index,
        Token::Star(index,) => *index,
        Token::Colon(index,) => *index,
        Token::Semicolon(index,) => *index,
        Token::Varargs(index,) => *index,
        Token::Identifier(index,_) => *index,
        Token::StringLiteral(index,_) => *index,
        Token::Number(index,_) => *index,
    }
}

pub fn parse(input: Vec<MacroToken>, context: &FragmentIterator) -> Result<cparser::parser::S, Option<Token>> {
    let tokens: Vec<Token> = input.iter().enumerate().map(|(i, t)| preprocessor_to_parser(&t.ty, i)).collect();
    let sources: Vec<Source> = input.into_iter().map(|t| t.source).collect();
    //for t in &tokens {
    //    print!("{:?}", t);
    //}
    match cparser::parse(tokens) {
        Err(Some(token)) => {
            let index = get_source_index(&token);
            println!("\nCaused by:\n{}", context.source_to_str(&sources[index]));
            Err(Some(token))
        }
        any => any
    }
}
