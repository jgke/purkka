use std::str::Chars;
use tokentype::*;

fn peek_str(s: &Chars) -> Option<char> {
    s.as_str().chars().next()
}

pub fn read_number(c: char, s: &mut Chars) -> TokenType {
    let mut number = c.to_string();

    while let Some(c) = peek_str(s) {
        match c {
            '0' ... '9' => {
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
            '0' ... '9' | 'a' ... 'z' | 'A' ... 'Z' | '_' => {
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

fn num_val(c: char) -> u8 {
    return c as u8 - '0' as u8;
}

fn char_from_octal(c1: char, c2: char, c3: char) -> char {
    return (8*8*num_val(c1) + 8*num_val(c2) + num_val(c3)) as char;
}

fn get_octal(c: char, s: &mut Chars) -> char {
    match peek_str(s) {
        Some(second @ '0'...'7') => {
            s.next();
            match peek_str(s) {
                Some(third @ '0'...'7') => {
                    s.next();
                    return char_from_octal(c, second, third);
                }
                _ => return char_from_octal('0', c, second)
            }
        }
        _ => return char_from_octal('0', '0', c)
    }
}

fn get_hex(s: &mut Chars) -> char {
    let mut num: u8 = 0;
    while let Some(c) = peek_str(s) {
        match c {
            c @ '0' ... '9' => {
                num = num.saturating_mul(16).saturating_add(num_val(c));
                s.next();
            }
            c @ 'a' ... 'f' => {
                num = num.saturating_mul(16).saturating_add(c as u8 - 'a' as u8 + 10);
                s.next();
            }
            c @ 'A' ... 'F' => {
                num = num.saturating_mul(16).saturating_add(c as u8 - 'A' as u8 + 10);
                s.next();
            }
            _ => break
        }
    }
    return (num & 255) as u8 as char;
}

pub fn read_string(s: &mut Chars) -> TokenType {
    let mut content = String::from("");

    while let Some(c) = peek_str(s) {
        s.next();
        match c {
            '"' => return TokenType::StringLiteral(content),
            '\\' => {
                match s.next() {
                    Some('\'') => content.push('\''),
                    Some('"') => content.push('\"'),
                    Some('?') => content.push('?'),
                    Some('\\') => content.push('\\'),
                    Some('a') => content.push('\x07'),
                    Some('b') => content.push('\x08'),
                    Some('f') => content.push('\x0C'),
                    Some('n') => content.push('\n'),
                    Some('t') => content.push('\t'),
                    Some('r') => content.push('\r'),
                    Some('v') => content.push('\x0B'),
                    Some(c @ '0'...'7') => content.push(get_octal(c, s)),
                    Some('x') => content.push(get_hex(s)),
                    Some(c) => content.push(c),
                    _ => panic!("Parse error")
                }
            }
            _ => content.push(c)
        };
    }

    panic!("Parse error");
}

fn read_until(c: char, s: &mut Chars) -> String {
    let mut str = String::from("");
    while let Some(cc) = s.next() {
        if c == cc {
            break;
        }
        str.push(cc);
    }
    return str;
}

fn read_macro(s: &mut Chars) -> TokenType {
    let str = s.as_str();
    for (c_macro, m) in SIMPLE_MACROS.iter() {
        if str.starts_with(c_macro) {
            return TokenType::Macro(m);
        }
    }

    if str.starts_with("include") {
        for _ in 0.."include".len() {
            s.next();
        }
        read_whitespace(s);
        match s.next() {
            Some('<') => return TokenType::MacroInclude(MacroInclude::IncludeSystem(read_until('>', s))),
            Some('"') => return TokenType::MacroInclude(MacroInclude::IncludeLocal(read_until('"', s))),
            _ => panic!("Parse error")
        }
    }

    panic!("Parse error");
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
                '0' ... '9' => Some(read_number(c, s)),
                'a' ... 'z' | 'A' ... 'Z' | '_' => Some(read_identifier(c, s)),
                ' ' | '\t' | '\n' => Some(read_whitespace(s)),
                '"' => Some(read_string(s)),
                '#' => Some(read_macro(s)),
                _ => match_operator(c, s)
            }
        }
        None => None
    }
}
