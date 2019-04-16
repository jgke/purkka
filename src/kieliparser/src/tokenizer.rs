use crate::token::{Token, KEYWORDS, TOKEN_TYPES};

use fragment::fragment::{FragmentIterator, Source};
use shared::intern::StringInterner;
use shared::utils::*;

pub fn tokenize(content: &str) -> (Vec<Token>, StringInterner) {
    let mut iter = FragmentIterator::new("file", content);
    let mut intern = StringInterner::new();
    let mut res = Vec::new();
    while iter.peek().is_some() {
        let (token, _source) = match iter.peek().unwrap() {
            ' ' | '\t' | '\n' => { iter.next(); continue; }
            '"' => read_string(&mut iter, &mut intern),
            '\'' => read_char(&mut iter),
            'a'...'z' | 'A'...'Z' | '_' => read_identifier(&mut iter, &mut intern),
            '0'...'9' => read_number(&mut iter, &mut intern),
            _ => read_other(&mut iter, &mut intern),
        };
        res.push(token);
    }
    (res, intern)
}

fn read_number(iter: &mut FragmentIterator, intern: &mut StringInterner) -> (Token, Source) {
    let (content, source) = iter.collect_while(|c| {
        match c {
            '0'...'9' | '_' | '.' => true,
            _ => false,
        }});
    if !content.contains('.') {
        (Token::Integer(content.parse().unwrap()), source)
    } else {
        (Token::Float(intern.get_ref(&content)), source)
    }
}

fn read_string(iter: &mut FragmentIterator, intern: &mut StringInterner) -> (Token, Source) {
    let mut end = false;
    let (content, source) = iter.collect_while_flatmap(|c, iter| match c {
        '"' => {
            if end {
                iter.next();
                None
            } else {
                end = true;
                Some(vec![])
            }
        }
        '\n' => panic!("Missing terminating \" character"),
        '\\' => {
            iter.next();
            if let Some(c) = iter.peek() {
                Some(vec![read_string_escape(iter, c)])
            } else {
                panic!("Unexpected end of file");
            }
        }
        c => Some(vec![c]),
    });
    (Token::StringLiteral(intern.get_ref(&content.into_iter().map(|(_, c)| c).collect::<String>())), source)
}

fn read_char(iter: &mut FragmentIterator) -> (Token, Source) {
    let mut end = false;
    let (content, source) = iter.collect_while_flatmap(|c, iter| match c {
        '\'' => {
            if end {
                iter.next();
                None
            } else {
                end = true;
                Some(vec![])
            }
        }
        '\n' => panic!("Missing terminating \' character"),
        '\\' => {
            iter.next();
            if let Some(c) = iter.peek() {
                Some(vec![read_string_escape(iter, c)])
            } else {
                panic!("Unexpected end of file");
            }
        }
        c => Some(vec![c]),
    });
    assert!(content.len() == 1);
    (Token::Char(content[0].1), source)
}

fn read_string_escape(iter: &mut FragmentIterator, c: char) -> char {
    match c {
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
        c @ '0'...'7' => get_octal(iter, c),
        'x' => get_hex(iter),
        c => c,
    }
}

fn get_octal(iter: &mut FragmentIterator, c: char) -> char {
    iter.next();
    match iter.peek() {
        Some(second @ '0'...'7') => {
            iter.next();
            match iter.peek() {
                Some(third @ '0'...'7') => {
                    iter.next();
                    return char_from_octal(c, second, third);
                }
                _ => return char_from_octal('0', c, second),
            }
        }
        _ => return char_from_octal('0', '0', c),
    }
}

fn get_hex(iter: &mut FragmentIterator) -> char {
    let mut num: u8 = 0;
    while let Some(c) = iter.peek() {
        match c {
            c @ '0'...'9' => {
                num = num.saturating_mul(16).saturating_add(num_val(c));
                iter.next();
            }
            c @ 'a'...'f' => {
                num = num
                    .saturating_mul(16)
                    .saturating_add(c as u8 - 'a' as u8 + 10);
                iter.next();
            }
            c @ 'A'...'F' => {
                num = num
                    .saturating_mul(16)
                    .saturating_add(c as u8 - 'A' as u8 + 10);
                iter.next();
            }
            _ => break,
        }
    }
    return (num & 255) as u8 as char;
}

fn read_identifier(iter: &mut FragmentIterator, intern: &mut StringInterner) -> (Token, Source) {
    let (content, source) = iter.collect_while(|c| match c {
        '0'...'9' | 'a'...'z' | 'A'...'Z' | '_' => true,
        _ => false,
    });

    for (keyword, k) in KEYWORDS.iter() {
        if &content == *keyword {
            return (k(), iter.current_source());
        }
    }

    (Token::Identifier(intern.get_ref(&content)), source)
}

fn read_other(iter: &mut FragmentIterator, intern: &mut StringInterner) -> (Token, Source) {
    for (punctuation, p) in TOKEN_TYPES.iter() {
        if iter.starts_with(punctuation) {
            iter.next_new_span();
            for _ in 1..punctuation.len() {
                iter.next();
            }
            return (p(), iter.current_source());
        }
    }

    let (content, source) = iter.collect_while(|c| {
        match c {
            '!'|'#'|'$'|'%'|'&'|'*'|'+'|'.'|'/'|'<'|'='|'>'|'?'|'@'|'\\'|'^'|'|'|'-'|'~' => true,
            _ => false,
        }});
    if content.len() > 0 {
        (Token::Operator(intern.get_ref(&content)), source)
    } else {
        panic!("Unexpected character: {}", iter.next().unwrap());
    }
}
