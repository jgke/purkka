use std::str::Chars;
use tokentype::*;

fn peek_str(s: &Chars) -> Option<char> {
    s.as_str().chars().next()
}

pub fn read_number(c: char, s: &mut Chars) -> TokenType {
    let mut number = c.to_string();

    while let Some(c) = peek_str(s) {
        match c {
            '0' ..= '9' => {
                number.push(c);
                s.next();
            },
            _ => break
        }

    }

    TokenType::Constant(Constant::Integer(number))
}

pub fn read_identifier(c: char, s: &mut Chars) -> TokenType {
    let mut identifier = c.to_string();

    while let Some(c) = peek_str(s) {
        match c {
            '0' ..= '9' | 'a' ..= 'z' | 'A' ..= 'Z' => {
                identifier.push(c);
                s.next();
            }
            _ => break
        };
    }

    for (keyword, kw) in KEYWORDS.iter() {
        if keyword.to_string() == identifier {
            return TokenType::Keyword(kw);
        }
    }


    TokenType::Identifier(identifier)
}

pub fn read_whitespace(s: &mut Chars) -> TokenType {
    while let Some(c) = peek_str(s) {
        match c {
            ' ' | '\t' | '\n' => {
                s.next();
            }
            _ => break
        };
    }

    TokenType::Whitespace
}

fn match_operator(c: char, s: &mut Chars) -> Option<TokenType> {
    for (operator, op) in OPERATORS.iter() {
        if operator.starts_with(c) && s.as_str().starts_with(&operator[1..]) {
            for _ in 1..operator.len() {
                s.next();
            }
            return Some(TokenType::Operator(op));
        }
    }
    for (punctuation, p) in PUNCTUATION.iter() {
        if punctuation.starts_with(c) && s.as_str().starts_with(&punctuation[1..]) {
            for _ in 1..punctuation.len() {
                s.next();
            }
            return Some(TokenType::Punctuation(p));
        }
    }
    None
}

pub fn read_token(s: &mut Chars) -> Option<TokenType> {
    match s.next() {
        Some(c) => {
            match c {
                '0' ..= '9' => Some(read_number(c, s)),
                'a' ..= 'z' | 'A' ..= 'Z' => Some(read_identifier(c, s)),
                ' ' | '\t' | '\n' => Some(read_whitespace(s)),
                _ => match_operator(c, s)
            }
        }
        None => None
    }
}
