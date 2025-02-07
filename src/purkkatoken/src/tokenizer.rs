use crate::token::{Token, KEYWORDS, TOKEN_TYPES};

use fragment::fragment::{FragmentIterator, Source};
use shared::intern::StringInterner;
use shared::utils::*;

pub fn tokenize(
    content: &str,
    filename: &str,
) -> (Vec<Token>, StringInterner, FragmentIterator, Vec<Source>) {
    let mut iter = FragmentIterator::new(filename, content);
    let mut intern = StringInterner::new();
    let mut res = Vec::new();
    let mut sources = Vec::new();
    while iter.peek().is_some() {
        if iter.starts_with("//") || iter.starts_with("/*") {
            flush_comment(&mut iter);
            continue;
        }
        let (token, source) = match iter.peek().unwrap() {
            ' ' | '\t' | '\n' => {
                iter.next();
                continue;
            }
            '"' => read_string(&mut iter, &mut intern, res.len()),
            '\'' => read_char(&mut iter, res.len()),
            'a'..='z' | 'A'..='Z' | '_' => read_identifier(&mut iter, &mut intern, res.len()),
            '0'..='9' => read_number(&mut iter, &mut intern, res.len()),
            _ => read_other(&mut iter, &mut intern, res.len()),
        };
        res.push(token);
        sources.push(source);
    }
    (res, intern, iter, sources)
}

fn flush_comment(iter: &mut FragmentIterator) {
    let ty = iter.peek_n(2);
    iter.next();
    iter.next();
    match ty.as_ref() {
        "//" => {
            iter.collect_while(|x| x != '\n');
        }
        "/*" => {
            let mut prev_star = false;
            let mut stop_next = false;
            iter.collect_while(|x| {
                if stop_next {
                    return false;
                }
                if x == '*' {
                    prev_star = true;
                } else if x == '/' && prev_star {
                    stop_next = true;
                } else {
                    prev_star = false;
                }
                true
            });
        }
        t => panic!("Unexpected start of comment: {}", t),
    }
}

fn read_number(
    iter: &mut FragmentIterator,
    intern: &mut StringInterner,
    num: usize,
) -> (Token, Source) {
    let radix_str = iter.peek_n(2);
    let radix = match radix_str.as_ref() {
        "0b" => {
            iter.next();
            iter.next();
            2
        }
        "0x" => {
            iter.next();
            iter.next();
            16
        }
        _ => 10,
    };
    let mut has_exponent = false;
    let (content_str, source) = iter.collect_while(
        |c| if has_exponent {
            has_exponent = false;
            match c {
                '+' | '-' => true,
                c => panic!("Unexpected character in floating point literal: {}", c)
            }
        } else {
            match c {
                '0'..='9' | '_' | '.' => true,
                'a'..='f' | 'A'..='F' if radix == 16 => true,
                'e' => { has_exponent = true; true } 
                _ => false,
            }
        });
    let content = content_str
        .chars()
        .filter(|c| *c != '_')
        .collect::<String>();
    if !content.contains('.') && !content.contains('e') {
        (
            Token::Integer(num, i128::from_str_radix(&content, radix).unwrap()),
            source,
        )
    } else {
        (Token::Float(num, intern.get_ref(&content)), source)
    }
}

fn read_string(
    iter: &mut FragmentIterator,
    intern: &mut StringInterner,
    num: usize,
) -> (Token, Source) {
    match iter.next_new_span() {
        Some('"') => {}
        Some(t) => panic!("Unexpected character: {}", t),
        None => panic!("Unexpected end of file"),
    }

    let mut content = String::new();

    loop {
        match iter.next() {
            Some('"') => {
                break;
            }
            Some('\n') => panic!("Missing terminating \" character"),
            Some('\\') => {
                if let Some(c) = iter.peek() {
                    content.push(read_string_escape(iter, c));
                } else {
                    panic!("Unexpected end of file");
                }
            }
            Some(c) => content.push(c),
            None => panic!("Unexpected end of file"),
        }
    }
    (
        Token::StringLiteral(num, intern.get_ref(&content)),
        iter.current_source(),
    )
}

fn read_char(iter: &mut FragmentIterator, num: usize) -> (Token, Source) {
    match iter.next_new_span() {
        Some('\'') => {}
        Some(t) => panic!("Unexpected character: {}", t),
        None => panic!("Unexpected end of file"),
    }
    let c = match iter.next() {
        Some('\\') => {
            if let Some(c) = iter.peek() {
                read_string_escape(iter, c)
            } else {
                panic!("Unexpected end of file");
            }
        }
        Some('\n') => panic!("Unexpected newline"),
        Some(c) => c,
        None => panic!("Unexpected end of file"),
    };
    match iter.next() {
        Some('\'') => {}
        Some(t) => panic!("Unexpected character: {}", t),
        None => panic!("Unexpected end of file"),
    }
    (Token::Char(num, c), iter.current_source())
}

fn read_string_escape(iter: &mut FragmentIterator, c: char) -> char {
    let ret = match c {
        '\'' => '\'',
        '"' => '\"',
        '?' => '?',
        '\\' => '\\',
        'a' => '\x07',
        'b' => '\x08',
        'f' => '\x0C',
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        'v' => '\x0B',
        c @ '0'..='7' => return get_octal(iter, c),
        'x' => return get_hex(iter),
        c => c,
    };
    iter.next();
    ret
}

fn get_octal(iter: &mut FragmentIterator, c: char) -> char {
    iter.next();
    match iter.peek() {
        Some(second @ '0'..='7') => {
            iter.next();
            match iter.peek() {
                Some(third @ '0'..='7') => {
                    iter.next();
                    char_from_octal(c, second, third)
                }
                _ => char_from_octal('0', c, second),
            }
        }
        _ => char_from_octal('0', '0', c),
    }
}

#[allow(clippy::char_lit_as_u8)]
fn get_hex(iter: &mut FragmentIterator) -> char {
    let mut num: u8 = 0;
    while let Some(c) = iter.peek() {
        match c {
            c @ '0'..='9' => {
                num = num.saturating_mul(16).saturating_add(num_val(c));
                iter.next();
            }
            c @ 'a'..='f' => {
                num = num
                    .saturating_mul(16)
                    .saturating_add(c as u8 - 'a' as u8 + 10);
                iter.next();
            }
            c @ 'A'..='F' => {
                num = num
                    .saturating_mul(16)
                    .saturating_add(c as u8 - 'A' as u8 + 10);
                iter.next();
            }
            _ => break,
        }
    }
    num as char
}

fn read_identifier(
    iter: &mut FragmentIterator,
    intern: &mut StringInterner,
    num: usize,
) -> (Token, Source) {
    let (content, source) = iter.collect_while(|c| match c {
        '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => true,
        _ => false,
    });

    for (keyword, k) in KEYWORDS.iter() {
        if content == *keyword {
            return (k(num), iter.current_source());
        }
    }

    (Token::Identifier(num, intern.get_ref(&content)), source)
}

fn read_other(
    iter: &mut FragmentIterator,
    intern: &mut StringInterner,
    num: usize,
) -> (Token, Source) {
    for (punctuation, p) in TOKEN_TYPES.iter() {
        if iter.starts_with(punctuation) {
            iter.next_new_span();
            for _ in 1..punctuation.len() {
                iter.next();
            }
            return (p(num), iter.current_source());
        }
    }

    let peeked = iter.peek();

    let (content, source) = iter.collect_while(|c| match c {
        '!' | '#' | '$' | '%' | '&' | '*' | '+' | '/' | '<' | '=' | '>' | '?' | '@' | '\\'
        | '^' | '|' | '-' | '~' => true,
        _ => false,
    });
    if !content.is_empty() {
        (Token::Operator(num, intern.get_ref(&content)), source)
    } else {
        panic!("Unexpected character: '{}'", peeked.unwrap());
    }
}
