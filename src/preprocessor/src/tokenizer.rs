use tokentype::{OPERATORS, PUNCTUATION, Operator, Punctuation};

use std::collections::{HashMap, HashSet};
use std::fmt;

use shared::utils::*;
use shared::fragment::{FragmentIterator, Source};

#[derive(Debug)]
pub struct Output {
    pub macro_functions: HashMap<String, ()>,
    pub output: String
}

#[derive(Clone, Debug)]
enum Macro {
    Text(Vec<MacroToken>),
    Function(Vec<String>, Vec<MacroToken>)
}

pub(crate) struct MacroContext {
    symbols: HashMap<String, Macro>,
    if_stack: Vec<Vec<bool>>,
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
    pub fn display<'a>(&'a self, iter: &'a FragmentIterator) -> MacroTokenDisplay<'a> {
        MacroTokenDisplay { token: self, iter }
    }
}

pub struct MacroTokenDisplay<'a> {
    token: &'a MacroToken,
    iter: &'a FragmentIterator
}

impl fmt::Display for MacroTokenDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type: {:?}\n{}", self.token.ty, self.iter.source_to_str(&self.token.source))
    }
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
                        out.extend(self.maybe_expand_identifier(token, iter).into_iter())
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

    /// Parse a macro (eg. #define FOO BAR) and store it for later, or act instantly in case of #if
    fn read_macro(&mut self, iter: &mut FragmentIterator) -> Vec<MacroToken> {
        let (row_string, total_span) = self.preprocess_get_macro_line(iter);
        let file = iter.current_filename();
        let mut sub_iter = FragmentIterator::with_offset(&file, &row_string, total_span.span.lo);
        let (ty, _) = self.read_identifier_raw(&mut sub_iter);
        match ty.as_str() {
            "define" => {
                let (left, _) = self.read_identifier_raw(&mut sub_iter);
                if let Some('(') = sub_iter.peek() {
                    assert_eq!(self.get_token(&mut sub_iter, false).0.map(|x| x.ty), Some(MacroTokenType::Punctuation(Punctuation::OpenParen)));
                    let mut depth = 0;
                    let mut args = Vec::new();
                    let mut had_comma = None;
                    while sub_iter.peek().is_some() {
                        if let (Some(mut token), subspan) = self.get_token(&mut sub_iter, false) {
                            match (had_comma, token.ty.clone()) {
                                (None, MacroTokenType::Punctuation(Punctuation::CloseParen)) => break,
                                (Some(false), MacroTokenType::Punctuation(Punctuation::CloseParen)) => break,
                                (None, MacroTokenType::Identifier(ident)) => {
                                    args.push(ident);
                                    had_comma = Some(false);
                                }
                                (Some(true), MacroTokenType::Identifier(ident)) => {
                                    args.push(ident);
                                    had_comma = Some(false);
                                }
                                (Some(false), MacroTokenType::Punctuation(Punctuation::Comma)) => {
                                    had_comma = Some(true);
                                }
                                _ => panic!("Unexpected token: {:?}", token.ty)
                            }
                        }
                    }

                    let mut right = Vec::new();

                    while sub_iter.peek().is_some() {
                        if let (Some(mut token), subspan) = self.get_token(&mut sub_iter, false) {
                            token.respan(&total_span);
                            right.push(token);
                        }
                    }

                    self.symbols.insert(left, Macro::Function(args, right));
                } else {
                    let mut right = Vec::new();

                    while sub_iter.peek().is_some() {
                        if let (Some(mut token), subspan) = self.get_token(&mut sub_iter, false) {
                            token.respan(&total_span);
                            right.push(token);
                        }
                    }

                    self.symbols.insert(left, Macro::Text(right));
                }
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

    /// Expand an identifier, if it is a macro
    fn maybe_expand_identifier(&mut self, token: MacroToken, iter: &mut FragmentIterator) -> Vec<MacroToken> {
        let mut empty_set = HashSet::new();
        let result = self.maybe_expand_identifier_recur(token, iter, &mut empty_set);
        assert!(empty_set.len() == 0);
        result
    }

    fn maybe_expand_identifier_recur(&mut self, token: MacroToken,
                                     iter: &mut FragmentIterator,
                                     used_names: &mut HashSet<String>) -> Vec<MacroToken> {
        match &token.ty.clone() {
            MacroTokenType::Identifier(ident) => {
                if used_names.get(ident).is_some() {
                    return vec![token]
                }
                let maybe_syms = self.symbols.get(ident).map(|x| x.clone());
                match maybe_syms {
                    Some(Macro::Text(syms)) =>
                        self.expand_text_macro(iter, ident, &token.source, &syms, used_names),
                    Some(Macro::Function(args, syms)) =>
                        self.expand_function_macro(iter, ident, &token.source, &args, &syms, used_names),
                    None => vec![token]
                }
            }
            _ => vec![token],
        }
    }

    fn expand_text_macro(&mut self,
                         iter: &mut FragmentIterator,
                         ident: &str, source: &Source,
                         syms: &Vec<MacroToken>, used_names: &mut HashSet<String>) -> Vec<MacroToken> {
        used_names.insert(ident.to_string());
        let out = syms.iter().flat_map(|t| {
            let mut out_token = t.clone();
            out_token.respan(source);
            let result = self.maybe_expand_identifier_recur(out_token, iter, used_names).into_iter();
            result
        }).collect();
        used_names.remove(ident);
        out 
    }

    fn expand_function_macro(&mut self, iter: &mut FragmentIterator,
                             ident: &str, source: &Source,
                             fn_args: &Vec<String>, syms: &Vec<MacroToken>,
                             used_names: &mut HashSet<String>) -> Vec<MacroToken> {
        let (mut more_syms, total_span) = self.parse_new_function_macro(iter, source, fn_args);
        let mut body_span = syms.get(0).map(|x| x.source.clone()).unwrap_or(total_span.clone());
        let mut sub_iter = syms.iter().for_each(|t| {
            body_span.span.hi = t.source.span.hi;
        });
        let mut sub_iter = syms.iter().map(|t| {
            let mut out_token = t.clone();
            out_token.respan(&total_span);
            out_token
        }).peekable();
        let mut res = Vec::new();

        println!("body_span: {}", iter.source_to_str(&body_span));
        println!("total_span: {}", iter.source_to_str(&total_span));
        println!("fn_args: {:?}", fn_args);
        println!("syms: {:?}", syms);
        println!("used_names: {:?}", used_names);
        println!("more_syms: {:?}", more_syms);

        while let Some(t) = sub_iter.next() {
            let outer_source = t.source.clone();
            println!("token: {}", t.display(iter));
            match t.ty.clone() {
                MacroTokenType::Identifier(ident) => {
                    let syms = more_syms
                        .get(&ident)
                        .map(|x| Macro::Text(x.to_vec()))
                        .or_else(|| self.symbols.get(&ident).map(|x| x.clone()));
                    println!("syms: {:?}", syms);
                    match syms {
                        Some(Macro::Text(tt)) =>
                            res.append(&mut tt.iter()
                                       .map(|x| {
                                           let mut nx = x.clone();
                                           nx.source.merge(&t.source);
                                           nx
                                       }).collect()
                                ),
                        Some(Macro::Function(body_args, tt)) => {
                            let (extra_tokens, some_src) = self.parse_function_macro_args(&mut sub_iter, &outer_source, &body_args);
                            for t in tt {
                                match t.ty.clone() {
                                    MacroTokenType::Identifier(ident) => {
                                        let mut default = vec![t.clone()];
                                        res.append(
                                            &mut more_syms.get(&ident)
                                            .unwrap_or(&default)
                                            .iter()
                                            .map(|sym| {
                                                let mut n_sym = sym.clone();
                                                n_sym.source.merge(&outer_source);
                                                n_sym.source.merge(&t.source);
                                                n_sym
                                            }).collect())
                                    }
                                    _ => {
                                        let mut n_sym = t.clone();
                                        res.push(n_sym)
                                    }
                                }
                            }
                        }
                        None => {
                            let mut n_sym = t.clone();
                            res.push(n_sym)
                        }
                    }
                }
                _ => {
                    let mut n_sym = t.clone();
                    res.push(n_sym)
                }
            }
        }
        res
    }

    fn parse_function_macro_args(&self, iter: &mut Iterator<Item=MacroToken>, start: &Source,
                                 args: &Vec<String>) -> (HashMap<String, Vec<MacroToken>>, Source) {
        let mut more_syms = HashMap::new();
        let mut depth = 0; // paren depth
        let mut arg_count = 0;
        let mut total_span = start.clone();
        let arg_len = args.len();
        let empty = "".to_string();
        if let Some(token) = iter.next() {
            match token.ty {
                MacroTokenType::Punctuation(Punctuation::OpenParen) => {
                    total_span.span.hi = token.source.span.hi;
                }
                _ => panic!("Unexpected token")
            }
        }

        'argfor: for arg in args.iter().chain(std::iter::once(&empty)) {
            let mut arg_vals: Vec<MacroToken> = Vec::new();
            while let Some(token) = iter.next() {
                total_span.span.hi = token.source.span.hi;
                match (depth, token.ty.clone()) {
                    (0, MacroTokenType::Punctuation(Punctuation::CloseParen)) => {
                        if arg != "" {
                            more_syms.insert(arg.clone(), arg_vals.drain(..).collect());
                            arg_count += 1;
                        }
                        break 'argfor;
                    }
                    (_, MacroTokenType::Punctuation(Punctuation::CloseParen)) => {
                        depth -= 1;
                        arg_vals.push(token)
                    }
                    (_, MacroTokenType::Punctuation(Punctuation::OpenParen)) => {
                        depth += 1;
                        arg_vals.push(token)
                    }
                    (0, MacroTokenType::Punctuation(Punctuation::Comma)) => {
                        more_syms.insert(arg.clone(), arg_vals.drain(..).collect());
                        arg_count += 1;
                    }
                    //(_, MacroTokenType::Identifier(ident)) => {
                    //    match self.symbols.get(&ident) {
                    //        Some(tt) => arg_vals.append(&mut tt),
                    //        None => arg_vals.push(token)
                    //    }
                    //}
                    _ => {
                        arg_vals.push(token)
                    }
                }
            }
        }
        if arg_count != arg_len {
            panic!("Incorrect number of arguments: expected {}, got {}", arg_len, arg_count);
        }
        (more_syms, total_span)
    }

    fn parse_new_function_macro(&mut self, iter: &mut FragmentIterator, start: &Source,
                                args: &Vec<String>) -> (HashMap<String, Vec<MacroToken>>, Source) {
        let mut more_syms = HashMap::new();
        let mut depth = 0; // paren depth
        let mut arg_count = 0;
        let mut total_span = start.clone();
        let arg_len = args.len();
        while iter.peek().is_some() {
            if let (Some(token), _) = self.get_token(iter, false) {
                match token.ty.clone() {
                    MacroTokenType::Punctuation(Punctuation::OpenParen) => {
                        break;
                    }
                    _ => unreachable!()
                }
            }
        }
        let empty = "".to_string();
        'argfor: for arg in args.iter().chain(std::iter::once(&empty)) {
            let mut arg_vals = Vec::new();
            'itersome: while iter.peek().is_some() {
                if let (Some(mut token), _) = self.get_token(iter, false) {
                    total_span.span.hi = token.source.span.hi;
                    match (depth, token.ty.clone()) {
                        (0, MacroTokenType::Punctuation(Punctuation::CloseParen)) => {
                            if arg != "" {
                                more_syms.insert(arg.clone(), arg_vals.drain(..).collect());
                                arg_count += 1;
                            }
                            break 'argfor;
                        }
                        (_, MacroTokenType::Punctuation(Punctuation::CloseParen)) => {
                            depth -= 1;
                            arg_vals.push(token)
                        }
                        (_, MacroTokenType::Punctuation(Punctuation::OpenParen)) => {
                            depth += 1;
                            arg_vals.push(token)
                        }
                        (0, MacroTokenType::Punctuation(Punctuation::Comma)) => {
                            more_syms.insert(arg.clone(), arg_vals.drain(..).collect());
                            arg_count += 1;
                        }
                        _ => {
                            arg_vals.push(token)
                        }
                    }
                }
            }
        }
        if arg_count != arg_len {
            panic!("Incorrect number of arguments: expected {}, got {}", arg_len, arg_count);
        }
        (more_syms, total_span)
    }

}
