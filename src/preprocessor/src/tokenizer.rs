use tokentype::{OPERATORS, PUNCTUATION, Operator, Punctuation};

use std::collections::{HashMap, HashSet};

use shared::utils::*;
use shared::fragment::{FragmentIterator, Source};

#[derive(Debug)]
pub struct Output {
    pub macro_functions: HashMap<String, ()>,
    pub output: String
}

pub(crate) struct MacroContext {
    symbols: HashMap<String, Vec<MacroToken>>,
    if_stack: Vec<Vec<bool>>,
    functions: HashMap<String, ()>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MacroTokenType {
    Identifier(String),
    Number(String),
    StringLiteral(String),
    Operator(Operator),
    Punctuation(Punctuation),
    Other(char),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MacroToken {
    pub source: Source,
    pub ty: MacroTokenType
}

impl MacroToken {
    fn respan(&mut self, source: &Source) {
        let old_source = self.source.clone();
        let mut new_source = source.clone();
        new_source.merge(&old_source);
        self.source = new_source;
    }
}

pub type ParseResult<T> = Result<T, &'static str>;

impl MacroContext {
    pub(crate) fn new() -> MacroContext {
        MacroContext {
            symbols: HashMap::new(),
            if_stack: Vec::new(),
            functions: HashMap::new()
        }
    }

    /// Divide src into MacroTokens.
    pub(crate) fn preprocess(&mut self, iter: &mut FragmentIterator) -> Vec<MacroToken> {
        let mut out: Vec<MacroToken> = Vec::new();
        let mut can_parse_macro = true;
        loop {
            match iter.peek() {
                Some('#') => {
                    if can_parse_macro {
                        out.append(&mut self.read_macro(iter));
                    } else {
                        panic!("Spurious #");
                    }
                }
                Some(_) => {
                    let ty = self.get_token(iter, can_parse_macro);
                    can_parse_macro = ty.1;
                    if let Some(token) = ty.0 {
                        out.extend(self.maybe_expand_identifier(token).into_iter())
                    }
                }
                None => break
            }
        }

        out
    }

    fn get_token(&mut self, iter: &mut FragmentIterator, parse_macro: bool) -> (Option<MacroToken>, bool) {
        let next = iter.peek_n(2);
        match next.as_ref() {
            "//" => {
                self.preprocess_get_macro_line(iter);
                (None, true)
            }
            "/*" => {
                iter.next(); // /
                iter.next(); // *
                self.preprocess_flush_until("*/", iter);
                (None, parse_macro)
            }
            "\\\n" => {
                iter.next(); // \
                iter.next(); // \n
                (None, parse_macro)
            }
            _ => match iter.peek() {
                Some('"') => (Some(self.read_string(iter)), false),
                Some('.')  => {
                    let substr = iter.peek_n(2);
                    let mut substr_iter = substr.chars();
                    substr_iter.next();
                    let token = if let Some(c) = substr_iter.next() {
                        match c {
                            '0' ... '9' => self.read_number(iter),
                            _ => self.read_other(iter)
                        }
                    } else {
                        self.read_other(iter)
                    };
                    (Some(token), false)
                }
                Some(' ') => { iter.next(); (None, parse_macro) },
                Some('\t') => { iter.next(); (None, parse_macro) },
                Some('\n') => {
                    iter.next();
                    (None, true)
                }
                Some(c) => {
                    let token = match c {
                        'a' ... 'z' | 'A' ... 'Z' | '_' => {
                            self.read_identifier(iter)
                        }
                        '0' ... '9' => {
                            self.read_number(iter)
                        }
                        _ => self.read_other(iter)
                    };
                    (Some(token), false)
                }
                None => (None, parse_macro)
            }
        }
    }

    fn preprocess_flush_until(&self, until: &str, iter: &mut FragmentIterator) -> (String, Source) {
        let mut out = iter.collect_while_map(
            |c, i| if i.starts_with(until) { None } else { Some(c) });
        for _  in 0..until.len() {
            match iter.next() {
                Some(c) => { out.0.push(c); }
                None => panic!("Unexpected end of file"),
            }
        }
        out
    }

    fn preprocess_get_macro_line(&self, iter: &mut FragmentIterator) -> (String, Source) {
        iter.collect_while_flatmap(
            |c, i| match c {
                '\\' => {
                    i.next();
                    match i.peek() {
                        Some('\n') => Some(vec![' ']),
                        Some(c) => Some(vec!['\\', c]),
                        _ => panic!("\\ at end of file")
                    }
                },
                '\n' => { i.next(); None },
                '\t' => Some(vec![' ']),
                c => Some(vec![c]),
            })
    }

    fn preprocess_get_string(&self, iter: &mut FragmentIterator) -> (String, Source) {
        let out = iter.collect_while_flatmap(
            |c, i| match (c, i.peek()) {
                ('\\', Some('"')) => {
                    i.next();
                    Some(vec!['\\', '"'])
                },
                ('\n', _) => None,
                ('\t', _) => Some(vec![' ']),
                (c, _) => Some(vec![c]),
            });
        iter.next(); // Newline
        out
    }

    fn read_identifier(&self, iter: &mut FragmentIterator) -> MacroToken {
        let (identifier, source) = self.read_identifier_raw(iter);
        MacroToken { source: source, ty: MacroTokenType::Identifier(identifier) }
    }

    fn read_identifier_raw(&self, iter: &mut FragmentIterator) -> (String, Source) {
        iter.collect_while(
                     |c| match c {
                         '0' ... '9' | 'a' ... 'z' | 'A' ... 'Z' | '_' => true,
                         _ => false
                     })
    }

    fn read_number(&self, iter: &mut FragmentIterator) -> MacroToken {
        let exponents = &["e+", "e-", "E+", "E-", "p+", "p-", "P+", "P-"];
        let (number, source) = iter.collect_while_flatmap(
            |c, i| exponents.iter()
            .filter(|e| i.starts_with(e))
            .last()
            .map(|e| {
                i.next();
                Some(e.chars().collect())
            })
            .unwrap_or_else(||
                            match c {
                                '0' ... '9' => Some(vec![c]),
                                '.' => Some(vec![c]),
                                'a' ... 'z' | 'A' ... 'Z' => Some(vec![c]),
                                _ => {
                                    None
                                }
                            }));
        MacroToken { source: source, ty: MacroTokenType::Number(number) }
    }

    fn read_string(&self, iter: &mut FragmentIterator) -> MacroToken {
        let mut end = false;
        let (content, source) = iter.collect_while_flatmap(
            |c, iter|
            match c {
                '"' => {
                    if end {
                        iter.next();
                        None
                    } else {
                        end = true;
                        Some(vec![])
                    }
                },
                '\n' => panic!("Missing terminating \" character"),
                '\\' => {
                    iter.next();
                    if let Some(c) = iter.peek() {
                        Some(vec![self.read_string_escape(iter, c)])
                    } else {
                        panic!("Unexpected end of file");
                    }
                },
                c => Some(vec![c])
            });
        MacroToken { source: source, ty: MacroTokenType::StringLiteral(content) }
    }

    fn read_string_escape(&self, iter: &mut FragmentIterator, c: char) -> char {
        match c {
            '\'' => '\'',
            '"' =>  '\"',
            '?' =>  '?',
            '\\' => '\\',
            'a' =>  '\x07',
            'b' =>  '\x08',
            'f' =>  '\x0C',
            'n' =>  '\n',
            't' =>  '\t',
            'r' =>  '\r',
            'v' =>  '\x0B',
            c @ '0'...'7' => self.get_octal(iter, c),
            'x' => self.get_hex(iter),
            c => c,
        }
    }

    fn get_octal(&self, iter: &mut FragmentIterator, c: char) -> char {
        match iter.peek() {
            Some(second @ '0'...'7') => {
                iter.next();
                match iter.peek() {
                    Some(third @ '0'...'7') => {
                        iter.next();
                        return char_from_octal(c, second, third);
                    }
                    _ => return char_from_octal('0', c, second)
                }
            }
            _ => return char_from_octal('0', '0', c)
        }
    }

    fn get_hex(&self, iter: &mut FragmentIterator) -> char {
        let mut num: u8 = 0;
        while let Some(c) = iter.peek() {
            match c {
                c @ '0' ... '9' => {
                    num = num.saturating_mul(16).saturating_add(num_val(c));
                    iter.next();
                }
                c @ 'a' ... 'f' => {
                    num = num.saturating_mul(16).saturating_add(c as u8 - 'a' as u8 + 10);
                    iter.next();
                }
                c @ 'A' ... 'F' => {
                    num = num.saturating_mul(16).saturating_add(c as u8 - 'A' as u8 + 10);
                    iter.next();
                }
                _ => break
            }
        }
        return (num & 255) as u8 as char;
    }

    fn read_other(&self, iter: &mut FragmentIterator) -> MacroToken {
        for (operator, op) in OPERATORS.iter() {
            if iter.starts_with(operator) {
                iter.next_new_span();
                for _ in 1..operator.len() {
                    iter.next();
                }
                return MacroToken { source: iter.current_source(), ty: MacroTokenType::Operator(**op) }
            }
        }

        for (punctuation, p) in PUNCTUATION.iter() {
            if iter.starts_with(punctuation) {
                iter.next_new_span();
                for _ in 1..punctuation.len() {
                    iter.next();
                }
                return MacroToken { source: iter.current_source(), ty: MacroTokenType::Punctuation(**p) }
            }
        }

        match iter.next_new_span() {
            Some(c) => MacroToken { source: iter.current_source(), ty: MacroTokenType::Other(c) },
            _ => unreachable!()
        }
    }

    fn read_macro(&mut self, iter: &mut FragmentIterator) -> Vec<MacroToken> {
        let (row_string, total_span) = self.preprocess_get_macro_line(iter);
        let file = iter.current_filename();
        let mut sub_iter = FragmentIterator::with_offset(&file, &row_string, total_span.span.lo);
        let (ty, _) = self.read_identifier_raw(&mut sub_iter);
        match ty.as_str() {
            "define" => {
                let (left, _) = self.read_identifier_raw(&mut sub_iter);
                if let Some('(') = sub_iter.peek() {
                    panic!("Cannot parse function macros yet");
                }

                let mut right = Vec::new();

                while sub_iter.peek().is_some() {
                    if let (Some(mut token), subspan) = self.get_token(&mut sub_iter, false) {
                        token.respan(&total_span);
                        right.push(token);
                    }
                }

                self.symbols.insert(left, right);
                vec![]
            },
            ty => panic!("Unknown macro type: {}", ty)
        }
    }

    fn macro_eval_flush_whitespace(&self, iter: &mut FragmentIterator) {
        while let Some(c) = iter.peek() {
            match c {
                ' ' | '\t' => iter.next(),
                _ => break
            };
        }
    }

    fn maybe_expand_identifier(&self, token: MacroToken) -> Vec<MacroToken> {
        let mut empty_set = HashSet::new();
        let result = self.maybe_expand_identifier_recur(token, &mut empty_set);
        assert!(empty_set.len() == 0);
        result
    }

    fn maybe_expand_identifier_recur(&self, token: MacroToken,
                                     used_names: &mut HashSet<String>) -> Vec<MacroToken> {
        match &token.ty.clone() {
            MacroTokenType::Identifier(ident) => {
                if used_names.get(ident).is_some() {
                    return vec![token]
                }
                let maybe_syms = self.symbols.get(ident);
                if let Some(syms) = maybe_syms {
                    println!("{:?}", syms);
                    syms.iter().flat_map(|t| {
                        let mut out_token = t.clone();
                        out_token.respan(&token.source);
                        used_names.insert(ident.to_string());
                        let result = self.maybe_expand_identifier_recur(out_token, used_names).into_iter();
                        used_names.remove(ident);
                        result
                    }).collect()
                } else {
                    vec![token]
                }
            }
            _ => vec![token]
        }
    }

    /*

    fn tokenize_macro_row(&self, mac: &str) -> Vec<MacroToken> {
        let mut out = Vec::new();
        let mut iter = mac.char_indices();
        loop {
            match (iter.next(), iter.peek()) {
                (Some((_, '#')), Some('#')) => {
                    iter.next();
                    out.push(MacroToken::TokenPaste)
                },
                (Some((_, '#')), _) => out.push(MacroToken::Stringify),
                (Some((_, ' ')), _) => {
                    self.macro_eval_flush_whitespace(&mut iter);
                    out.push(MacroToken::Whitespace)
                },
                (Some((_, c)), _) => match c {
                    'a' ... 'z' | 'A' ... 'Z' | '_' =>
                        out.push(MacroToken::Identifier(self.read_identifier(&mut iter, c))),
                    _ => out.push(MacroToken::Other(c.to_string()))
                }
                (None, _) => return out
            }
        }
    }

    fn tokenize_macro_row(&self, mac: &str) -> Vec<MacroToken> {
        let mut out = Vec::new();
        let mut iter = mac.char_indices();
        loop {
            match (iter.next(), iter.peek()) {
                (Some((_, '#')), Some('#')) => {
                    iter.next();
                    out.push(MacroToken::TokenPaste)
                },
                (Some((_, '#')), _) => out.push(MacroToken::Stringify),
                (Some((_, ' ')), _) => {
                    self.macro_eval_flush_whitespace(&mut iter);
                    out.push(MacroToken::Whitespace)
                },
                (Some((_, c)), _) => match c {
                    'a' ... 'z' | 'A' ... 'Z' | '_' =>
                        out.push(MacroToken::Identifier(self.read_identifier(&mut iter, c))),
                    _ => out.push(MacroToken::Other(c.to_string()))
                }
                (None, _) => return out
            }
        }
    }

    fn eval_constexpr(&mut self, tokens: &[MacroToken]) -> bool {
        let mut iter: VecDeque<MacroToken> = tokens.into_iter().map(|x| x.clone()).collect();
        match (iter.pop_front(), iter.front(), iter.get(1)) {
            (Some(MacroToken::Identifier(_)), None, _) => {
                unimplemented!();
            }
            _ => unimplemented!()
        }

        false
    }

    fn eval_macro(&mut self, mac: &str, iter: &mut FragmentIterator) -> String {
        let elif_regex = Regex::new(r"^\s*#\s*elif\s").unwrap();
        let else_regex = Regex::new(r"^\s*#\s*else([\s]|$)").unwrap();
        let endif_regex = Regex::new(r"^\s*#\s*endif([\s]|$)").unwrap();
        let row = self.tokenize_macro_row(mac);
        println!("eval: {:?}", row);
        match row.get(0) {
            Some(MacroToken::Identifier(ident)) => {
                match ident.as_ref() {
                    "define" => {
                        match row.get(1) {
                            Some(MacroToken::Whitespace) => {},
                            _ => panic!("Expected whitespace")
                        };
                        let name = match row.get(2) {
                            Some(MacroToken::Identifier(ident)) => {
                                ident
                            },
                            _ => panic!("Expected identifier")
                        };
                        // no whitespace before arg list
                        if let Some(MacroToken::Other("(".to_string())) = row.get(3) {
                            panic!("Function macros not supported");
                        };
                        let mut tokens = &row[3..];
                        self.symbols.insert(name.clone(), tokens.to_vec());
                        "\n".to_string()
                    }
                    t@"if" | t@"elif" => {
                        match row.get(1) {
                            Some(MacroToken::Whitespace) => {},
                            _ => panic!("Expected whitespace")
                        };
                        let constexpr_result = self.eval_constexpr(&row[2..]);
                        if t == "elif" {
                            self.if_stack.last_mut().unwrap().pop().unwrap();
                        }
                        self.if_stack.last_mut().unwrap().push(constexpr_result);
                        if !constexpr_result {
                            while iter.peek().is_some() &&
                                !any_match(iter.as_str(), &[&elif_regex, &else_regex, &endif_regex]) {
                                    let s = self.preprosess_get_macro_line(iter);
                                    println!("removed: {}", s);
                                }
                        }
                        "\n".to_string()
                    }
                    t@"ifdef" | t@"ifndef" => {
                        match row.get(1) {
                            Some(MacroToken::Whitespace) => {},
                            _ => panic!("Expected whitespace")
                        };
                        let name = match row.get(2) {
                            Some(MacroToken::Identifier(ident)) => {
                                ident
                            },
                            _ => panic!("Expected identifier")
                        };
                        assert!(else_regex.is_match("#else"));
                        assert!(!else_regex.is_match("#endif"));
                        let mut defined = self.symbols.contains_key(name);
                        if t == "ifndef" {
                            defined = !defined;
                        }
                        self.if_stack.last_mut().unwrap().push(defined);
                        if defined {
                            while iter.peek().is_some() && !else_regex.is_match(iter.as_str()) && !endif_regex.is_match(iter.as_str()) {
                                let s = self.preprosess_get_macro_line(iter);
                                println!("removed: {}", s);
                            }
                        }
                        " ".to_string()
                    }
                    "else" => {
                        if !self.if_stack.last().unwrap().last().unwrap() {
                            while iter.peek().is_some() && !endif_regex.is_match(iter.as_str()) {
                                let s = self.preprosess_get_macro_line(iter);
                                println!("removed: {}", s);
                            }
                        }
                        " ".to_string()
                    }
                    "endif" => {
                        self.if_stack.last_mut().unwrap().pop();
                        " ".to_string()
                    }
                    _ => panic!("Unknown macro")
                }
            },
            _ => panic!("Parse error")
        }
    }

    fn expand_macro_vec(&self, tokens: Vec<MacroToken>) -> String {
        let mut out = String::new();
        println!("expand: {:?}", tokens);
        let mut iter: VecDeque<MacroToken> = tokens.into_iter().collect();
        while let Some(MacroToken::Whitespace) = iter.front() {
                iter.pop_front(); // whitespace
        }

        loop {
            println!("{:?}", iter);
            match (iter.pop_front(), iter.front(), iter.get(1)) {
                (Some(MacroToken::Identifier(s)), Some(MacroToken::TokenPaste), _) => {
                    iter.pop_front(); // tokenpaste
                    while let Some(MacroToken::Whitespace) = iter.front() {
                        iter.pop_front(); // whitespace
                    }
                    match iter.pop_front() {
                        None => panic!("Paste token cannot be at the end of a macro"),
                        Some(MacroToken::Identifier(ss)) => {
                            iter.push_front(MacroToken::Identifier(format!("{}{}", s, ss)));
                        }
                        Some(MacroToken::Stringify) => {
                            panic!("Pasting an identifier and # is not allowed");
                        }
                        Some(MacroToken::TokenPaste) => {
                            // eg. #define A (B ## ## C)
                            // I'm not exactly sure what to do here, GCC's cpp seems to allow it
                            // (resulting in BC) so let's follow the example and push them back,
                            // so there's B ## C in the queue
                            iter.pop_front(); // pop off the ##
                            iter.push_front(MacroToken::Identifier(s)); // push back B
                        }
                        Some(MacroToken::Other(ss)) => {
                            // This is also an interesting case, anything can happen after here.
                            // At least the C standard is explicit about this. "If the result is
                            // not a valid preprocessing token, the behavior is undefined."
                            iter.push_front(MacroToken::Identifier(format!("{}{}", s, ss)));
                        }

                        // all whitespace is popped off so this is unreachable
                        Some(MacroToken::Whitespace) => unreachable!(),
                    }
                }
                (Some(MacroToken::Identifier(s)), Some(MacroToken::Whitespace),
                 Some(MacroToken::Identifier(_))) => out.push_str(&s),
                (Some(t@MacroToken::Identifier(_)), Some(MacroToken::Whitespace), _) => {
                    // an operator (or whitespace) is coming, pop off whitespace
                    // and push back the identifier
                    iter.pop_front();
                    iter.push_front(t);
                }
                (Some(MacroToken::Identifier(s)), _, _) => out.push_str(&s),
                (Some(MacroToken::Other(s)), _, _) => out.push_str(&s),
                (Some(MacroToken::Stringify), _, _) => out.push('#'),
                (Some(MacroToken::Whitespace), _, _) => out.push(' '),
                (Some(MacroToken::TokenPaste), _, _) => panic!("Token pasting cannot start a macro"),
                (None, _, _) => break,
            }
        }
        println!("expanded: {}", out);
        out
    }

    fn expand_macro(&self, identifier: &str, expanded_macros: &mut HashSet<String>) -> String {
        match self.symbols.get(identifier) {
            Some(tokens) => self.expand_macro_vec(tokens.to_vec()),
            None => identifier.to_string()
        }
    }
    */
}
