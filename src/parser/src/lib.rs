use ctoken::token::Token;
use preprocessor::macrotoken::{MacroToken, preprocessor_to_parser};
use fragment::fragment::{FragmentIterator};

fn get_source_index(token: &Token) -> usize {
    match token {
        Token::And(index,) => *index,
        Token::Arrow(index,) => *index,
        Token::Asm(index,..) => *index,
        Token::Assign(index,) => *index,
        Token::Auto(index,) => *index,
        Token::BitAnd(index,) => *index,
        Token::BitAndAssign(index,) => *index,
        Token::BitNot(index,) => *index,
        Token::BitOr(index,) => *index,
        Token::BitOrAssign(index,) => *index,
        Token::BitShiftLeft(index,) => *index,
        Token::BitShiftLeftAssign(index,) => *index,
        Token::BitShiftRight(index,) => *index,
        Token::BitShiftRightAssign(index,) => *index,
        Token::BitXor(index,) => *index,
        Token::BitXorAssign(index,) => *index,
        Token::Break(index,) => *index,
        Token::Case(index,) => *index,
        Token::Char(index,) => *index,
        Token::CharLiteral(index,..) => *index,
        Token::CloseBrace(index,) => *index,
        Token::CloseBracket(index,) => *index,
        Token::CloseParen(index,) => *index,
        Token::Colon(index,) => *index,
        Token::Comma(index,) => *index,
        Token::Const(index,) => *index,
        Token::Continue(index,) => *index,
        Token::Decrement(index,) => *index,
        Token::Default(index,) => *index,
        Token::DivAssign(index,) => *index,
        Token::Divide(index,) => *index,
        Token::Do(index,) => *index,
        Token::Dot(index,) => *index,
        Token::Double(index,) => *index,
        Token::Else(index,) => *index,
        Token::Enum(index,) => *index,
        Token::Equals(index,) => *index,
        Token::Extern(index,) => *index,
        Token::Float(index,) => *index,
        Token::For(index,) => *index,
        Token::Goto(index,) => *index,
        Token::Identifier(index,_) => *index,
        Token::If(index,) => *index,
        Token::Increment(index,) => *index,
        Token::Inline(index,) => *index,
        Token::Int(index,) => *index,
        Token::LessEqThan(index,) => *index,
        Token::LessThan(index,) => *index,
        Token::Long(index,) => *index,
        Token::Macro(..) => panic!("Macro token found: {:?}", token),
        Token::MacroPaste(..) => panic!("Macro token found: {:?}", token),
        Token::Minus(index,) => *index,
        Token::MinusAssign(index,) => *index,
        Token::Mod(index,) => *index,
        Token::ModAssign(index,) => *index,
        Token::MoreEqThan(index,) => *index,
        Token::MoreThan(index,) => *index,
        Token::Not(index,) => *index,
        Token::NotEquals(index,) => *index,
        Token::Number(index,_) => *index,
        Token::OpenBrace(index,) => *index,
        Token::OpenBracket(index,) => *index,
        Token::OpenParen(index,) => *index,
        Token::Or(index,) => *index,
        Token::Plus(index,) => *index,
        Token::PlusAssign(index,) => *index,
        Token::Register(index,) => *index,
        Token::Return(index,) => *index,
        Token::Semicolon(index,) => *index,
        Token::Short(index,) => *index,
        Token::Signed(index,) => *index,
        Token::Sizeof(index,_) => *index,
        Token::Static(index,) => *index,
        Token::StringLiteral(index,_) => *index,
        Token::Struct(index,) => *index,
        Token::Switch(index,) => *index,
        Token::Ternary(index,) => *index,
        Token::Times(index,) => *index,
        Token::TimesAssign(index,) => *index,
        Token::Typedef(index,) => *index,
        Token::Union(index,) => *index,
        Token::Unsigned(index,) => *index,
        Token::Varargs(index,) => *index,
        Token::Void(index,) => *index,
        Token::Volatile(index,) => *index,
        Token::While(index,) => *index,
    }
}

pub fn parse(input: Vec<MacroToken>, context: &FragmentIterator) -> Result<cparser::parser::S, Option<Token>> {
    let tokens: Vec<Token> = input.iter()
        .enumerate()
        .map(|(i, t)| preprocessor_to_parser(context, &t, i))
        .fold(Vec::new(), |mut list, t| {
            if let Some(Token::StringLiteral(_, s)) = list.last_mut() {
                if let Token::StringLiteral(_, ss) = t {
                    s.push_str(&ss);
                    return list;
                }
            }
            list.push(t);
            list
        });
    //for (t, s) in tokens.iter().zip(input.iter()) {
    //    println!("{:?} {}", t, s.to_src());
    //}
    println!("{:?}", tokens.last());
    match cparser::parse(tokens) {
        Err(Some(token)) => {
            let index = get_source_index(&token);
            println!("\nCaused by:\n{}", context.source_to_str(&input[index].source));
            Err(Some(token))
        }
        Ok(t) => {
            println!("Success!");
            Ok(t)
        }
        any => any
    }
}
