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
    Text(Source, Vec<MacroToken>),
    Function(Source, Vec<String>, Vec<MacroToken>)
}

pub(crate) struct MacroContext<CB> where CB: FnMut(String) -> String {
    symbols: HashMap<String, Macro>,
    get_file: CB
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

impl fmt::Debug for MacroTokenDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for MacroTokenDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type: {:?}\nsource:\n{}", self.token.ty, self.iter.source_to_str(&self.token.source))
    }
}

impl MacroToken {
    fn respan_front(&mut self, source: &Source) {
        let old_source = self.source.clone();
        let mut new_source = source.clone();
        new_source.merge(&old_source);
        self.source = new_source;
    }
    fn respan_back(&mut self, source: &Source) {
        self.source.merge(source)
    }

    fn get_identifier_str(&self) -> Option<String> {
        match &self.ty {
            MacroTokenType::Identifier(ident) => Some(ident.clone()),
            _ => None
        }
    }
}

pub type ParseResult<T> = Result<T, &'static str>;

struct MacroParseIter<'a>(&'a Vec<(MacroToken, HashSet<String>)>, usize, &'a mut FragmentIterator);

impl<CB> MacroContext<CB> where CB: FnMut(String) -> String {
    pub(crate) fn new(get_file: CB) -> MacroContext<CB> {
        MacroContext {
            symbols: HashMap::new(),
            get_file
        }
    }

    fn get_iterator(&mut self, filename: &str) -> FragmentIterator {
        let content = (self.get_file)(filename.to_string());
        FragmentIterator::new(filename, &content)
    }

    /// Divide src into MacroTokens.
    pub(crate) fn preprocess(&mut self, filename: &str) -> Vec<MacroToken> {
        let mut iter = self.get_iterator(filename);
        let mut out: Vec<MacroToken> = Vec::new();
        let mut can_parse_macro = true;
        loop {
            match iter.peek() {
                Some('#') => {
                    if can_parse_macro {
                        out.append(&mut self.read_macro(&mut iter));
                    } else {
                        panic!("Spurious #");
                    }
                }
                Some(_) => {
                    let ty = self.get_token(&mut iter, can_parse_macro);
                    can_parse_macro = ty.1;
                    if let Some(token) = ty.0 {
                        out.extend(self.maybe_expand_identifier(token, &mut iter).into_iter())
                    }
                }
                None => if !iter.advance_and_reset_span() { break }
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
        for (punctuation, p) in PUNCTUATION.iter() {
            if iter.starts_with(punctuation) {
                iter.next_new_span();
                for _ in 1..punctuation.len() {
                    iter.next();
                }
                return MacroToken { source: iter.current_source(), ty: MacroTokenType::Punctuation(**p) }
            }
        }

        for (operator, op) in OPERATORS.iter() {
            if iter.starts_with(operator) {
                iter.next_new_span();
                for _ in 1..operator.len() {
                    iter.next();
                }
                return MacroToken { source: iter.current_source(), ty: MacroTokenType::Operator(**op) }
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
                    let mut args = Vec::new();
                    let mut had_comma = None;
                    while sub_iter.peek().is_some() {
                        if let (Some(mut token), _) = self.get_token(&mut sub_iter, false) {
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
                        if let (Some(mut token), _) = self.get_token(&mut sub_iter, false) {
                            //token.respan_front(&total_span);
                            right.push(token);
                        }
                    }

                    self.symbols.insert(left, Macro::Function(total_span, args, right));
                } else {
                    let mut right = Vec::new();

                    while sub_iter.peek().is_some() {
                        if let (Some(mut token), _) = self.get_token(&mut sub_iter, false) {
                            //token.respan_front(&total_span);
                            right.push(token);
                        }
                    }

                    self.symbols.insert(left, Macro::Text(total_span, right));
                }
                vec![]
            },
            "include" => {
                loop {
                    match sub_iter.peek() {
                        Some(' ') | Some('\t') => sub_iter.next(),
                        _ => break
                    };
                }
                let start = self.read_other(&mut sub_iter);
                let is_quote = if let MacroTokenType::Other('"') = start.ty {
                    true
                } else if let MacroTokenType::Operator(Operator::LessThan) = start.ty {
                    false
                } else {
                    panic!("Unexpected character: {}", start.display(&sub_iter));
                };

                let (filename, _) = sub_iter.collect_while(|x|
                                                           (is_quote && x != '"') ||
                                                           (!is_quote && x != '>'));

                let end = self.read_other(&mut sub_iter);
                let is_quote = if let MacroTokenType::Other('"') = end.ty {
                    if !is_quote {
                        panic!("Invalid closing character")
                    }
                } else if let MacroTokenType::Operator(Operator::MoreThan) = end.ty {
                    if is_quote {
                        panic!("Invalid closing character")
                    }
                } else {
                    panic!("Unexpected character: {}", end.display(&sub_iter));
                };

                let content = (self.get_file)(filename.clone());
                iter.split_and_push_file(&filename, &content);

                vec![]
            }
            ty => panic!("Unknown macro type: {}", ty)
        }
    }

    // Macro utilities
    fn has_next_token(&self, ctx: &MacroParseIter) -> bool {
        let MacroParseIter(list, index, iter) = ctx;
        return list.len() > *index || iter.peek().is_some();
    }
    fn get_next_token(&mut self, ctx: &mut MacroParseIter) -> (Option<(MacroToken, HashSet<String>)>, bool) {
        let MacroParseIter(list, index, iter) = ctx;

        if list.len() > *index {
            return (Some(list[*index].clone()), true)
        }

        return (self.get_token(iter, false).0.map(|x| (x, HashSet::new())), false);
    }

    /// Expand an identifier, if it is a macro. Consumes tokens from `iter` in case of function
    /// macros. Does not consume tokens past fragment boundaries.
    ///
    /// Algorithm:
    /// ```text
    ///     let token list = [(t, {})]
    ///
    ///     for (t, used_macros) in token list
    ///         if t not in used_macros and (t, replacement list) is text macro:
    ///             add t to used macros
    ///
    ///             remove t from token list
    ///             for tt in replacement list:
    ///                 add (tt, used macros) to token list
    ///
    ///         else if t not in used_macros
    ///                 and (t, args, replacement list) is function macro
    ///                 and next token is (:
    ///             add t to used macros
    ///
    ///             read function macro arguments
    ///             //expand arguments
    ///             replace arguments in function macro body
    ///
    ///             remove t from token list
    ///             for tt in replacement list:
    ///                 add (tt, used macros) to token list
    ///
    ///         else:
    ///             advance iterator
    ///
    ///     return token list.map(|(x, _)| x)
    /// ```
    ///
    fn maybe_expand_identifier(&mut self, token: MacroToken, iter: &mut FragmentIterator) -> Vec<MacroToken> {
        let mut list: Vec<(MacroToken, HashSet<String>)> = vec![(token.clone(), HashSet::new())];
        let mut index = 0;

        'mainloop: while index < list.len() {
            let (t, used_names) = &list[index].clone();
            let ident_str = t.get_identifier_str();
            if ident_str.clone().map(|ident| used_names.contains(&ident)).unwrap_or(true) {
                index += 1;
                continue;
            } else {
                let maybe_syms = ident_str.clone()
                    .and_then(|ident| self.symbols.get(&ident))
                    .map(|x| x.clone());

                match maybe_syms {
                    Some(Macro::Text(span, body)) => {
                        // add t to used macros
                        //
                        // remove t from token list
                        // for tt in replacement list:
                        //     add (tt, used macros) to token list
                        let mut tokens = self.expand_text_macro(&ident_str.unwrap(), &span, &t.source, &body, used_names);

                        let mut rest = list.split_off(index+1);
                        list.remove(index);
                        list.append(&mut tokens);
                        list.append(&mut rest);
                    }
                    Some(Macro::Function(span, args, body)) => {
                        // else if t not in used_macros
                        //         and (t, args, replacement list) is function macro
                        //         and next token is (:
                        //         ...
                        //         remove t from token list
                        //         for tt in replacement list:
                        //             add (tt, used macros) to token list
                        loop {
                            // This part checks whether the next token is (. Since we're dealing
                            // with two iterators at once this is a bit complicated.
                            if self.has_next_token(&MacroParseIter(&list, index+1, iter)) {
                                let (next_tok, consumed_index) =
                                    self.get_next_token(&mut MacroParseIter(&list, index+1, iter));

                                match (next_tok.clone().map(|x| x.0.ty), consumed_index) {
                                    (Some(MacroTokenType::Punctuation(Punctuation::OpenParen)), true) => {
                                        break;
                                    }
                                    (Some(MacroTokenType::Punctuation(Punctuation::OpenParen)), false) => {
                                        list.push(next_tok.unwrap());
                                        break;
                                    },
                                    // we consumed whitespace from the iter in this case, so just
                                    // continue
                                    (None, false) => continue,
                                    // We got something else than whitespace or ( in these cases
                                    (Some(_), false) => {
                                        list.push(next_tok.unwrap());
                                        continue 'mainloop
                                    }
                                    (Some(_), true) => {
                                        index += 1;
                                        continue 'mainloop
                                    }
                                    (None, true) => unreachable!()
                                }
                            } else {
                                continue 'mainloop
                            }
                        }
                        let (consumed_count, mut tokens) = self.expand_function_macro(
                            &mut MacroParseIter(&list, index+2, iter),
                            &ident_str.unwrap(), &span, &t.source, &args, &body, used_names);

                        let mut rest = list.split_off(index+2);
                        list.remove(index+1); // open paren
                        list.remove(index); // function name
                        list.append(&mut tokens);
                        list.extend_from_slice(&rest[consumed_count..]);
                    }
                    None => {
                        index += 1;
                        continue 'mainloop;
                    }
                }
            }
        }

        list.into_iter().map(|(x, _)| x).collect()
    }

    /// Get the expanded form of ident.
    ///
    /// Algorithm part:
    ///     remove t from token list
    ///     for tt in replacement list:
    ///         add (tt, used macros) to token list
    fn expand_text_macro(&mut self,
                         ident: &str,
                         body_span: &Source,
                         ident_span: &Source,
                         body: &Vec<MacroToken>, used_names: &HashSet<String>) -> Vec<(MacroToken, HashSet<String>)> {
        let mut more_used_names = used_names.clone();
        more_used_names.insert(ident.to_string());
        let mut out = Vec::new();
        let mut iter = body.iter().peekable();
        while iter.peek().is_some() {
            let mut out_token = {
                let t = iter.next().unwrap();
                if iter.peek().map(|t| t.ty == MacroTokenType::Operator(Operator::MacroPaste)) == Some(true) {
                    iter.next(); // MacroPaste
                    let next = iter.next().unwrap();
                    combine_tokens(t, next)
                } else {
                    t.clone()
                }
            };
            out_token.respan_front(body_span);
            out_token.respan_front(ident_span);
            out.push((out_token, more_used_names.clone()))
        }
        out
    }

    /// Algorithm part:
    ///     add t to used macros
    ///
    ///     read function macro arguments
    ///     replace arguments in function macro body
    ///
    ///     remove t from token list
    ///     for tt in replacement list:
    ///         add (tt, used macros) to token list
    fn expand_function_macro(&mut self,
                             iter: &mut MacroParseIter,
                             ident: &str,
                             body_span: &Source,
                             args_span: &Source,
                             fn_args: &Vec<String>, body: &Vec<MacroToken>,
                             used_names: &HashSet<String>) -> (usize, Vec<(MacroToken, HashSet<String>)>) {
        // Example:
        // #define BAR(a) a
        // #define FOO(a) BAR(a) a
        // ~~~~~~~~~~~~~~~~~~~~~~~ body_span
        //         ~~~             ident
        //             ~           fn_args
        //                ~~~~~~~~ body
        // FOO(foo) expands to:
        //  foo
        //  Expanded from: FOO(foo)
        //  Expanded from: #define FOO(a) BAR(a) a
        //  Expanded from: #define BAR(a) a
        //  foo
        //  Expanded from: FOO(foo)
        //  Expanded from: #define FOO(a) BAR(a) a

        let mut used_syms = used_names.clone();
        used_syms.insert(ident.to_string());

        let (consume_count, more_syms, mut total_span, has_expanded) = self.parse_function_macro_arguments(iter, args_span, fn_args);
        total_span.span.source = None;
        let mut sub_iter = body.iter().peekable();
        let mut res: Vec<(MacroToken, HashSet<String>)> = Vec::new();

        while let Some(t) = sub_iter.next() {
            let outer_source = t.source.clone();
            match t.ty.clone() {
                MacroTokenType::Identifier(ident) => {
                    let syms = more_syms.get(&ident);
                    match syms {
                        Some(tt) =>
                            res.append(&mut tt.iter()
                                       .map(|x| {
                                           let mut nx = x.clone();
                                           if !has_expanded {
                                               nx.respan_back(&total_span);
                                           }
                                           (nx, used_syms.clone())
                                       }).collect()),
                        None => {
                            let mut n_sym = t.clone();
                            res.push((n_sym, used_syms.clone()))
                        }
                    }
                }
                _ => {
                    let mut n_sym = t.clone();
                    res.push((n_sym, used_syms.clone()))
                }
            }
        }
        let mut i = 0;
        while i < res.len() {
            if res.get(i+1).map(|t| t.0.ty == MacroTokenType::Operator(Operator::MacroPaste)) == Some(true) {
                let map = res[i].1.clone();
                let replacement = combine_tokens(&res[i].0, &res[i+2].0);
                shared::utils::remove_and_replace(&mut res, i..i+3, &mut vec![(replacement, map)]);
            }

            let ref mut t = res[i];
            t.0.respan_back(&body_span);
            i += 1;
        }
        (consume_count, res)
    }

    fn parse_function_macro_arguments(&mut self, iter: &mut MacroParseIter, start: &Source,
                                      args: &Vec<String>) -> (usize, HashMap<String, Vec<MacroToken>>, Source, bool) {
        let mut more_syms = HashMap::new();
        let mut depth = 0; // paren depth
        let mut arg_count = 0;
        let mut total_span = Some(start.clone());
        let mut consume_count = 0;
        let mut has_expanded = false;
        let arg_len = args.len();
        let empty = "".to_string();
        let empty_vec = vec![empty];
        let args_iter = if args.len() == 0 {
            &empty_vec
        } else {
            args
        };
        'argfor: for arg in args_iter.iter() {
            let mut arg_vals = Vec::new();
            'itersome: while self.has_next_token(iter) {
                if let (Some((mut token, used_names)), consumed_index) = self.get_next_token(iter) {
                    if consumed_index {
                        iter.1 += 1;
                        consume_count += 1;
                    }
                    if let Some(ref mut source) = total_span {
                        source.span.hi = token.source.span.hi;
                        if source.span.source.is_some() {
                            has_expanded = true;
                        }
                    } else {
                        total_span = Some(token.source.clone());
                    }
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
                            continue 'argfor;
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
        (consume_count, more_syms, total_span.unwrap(), has_expanded)
    }
}

fn combine_tokens(left: &MacroToken, right: &MacroToken) -> MacroToken {
    let left_str = left.get_identifier_str().unwrap();
    let right_str = right.get_identifier_str().unwrap();

    let mut source = left.source.clone();
    source.span.hi = right.source.span.hi;

    // This part feels like a bit of an overkill to parse a single token...
    let combined = format!("{}{}", left_str, right_str);
    let mut tmp_iter = FragmentIterator::new("", &combined);
    let mut parsed_token = MacroContext {
        get_file: unreachable_file_open,
        symbols: HashMap::new()
    }.get_token(&mut tmp_iter, false);

    assert!(tmp_iter.peek().is_none());
    assert!(parsed_token.0.is_some());

    MacroToken {
        source: source.bottom().clone(),
        ty: parsed_token.0.unwrap().ty
    }
}

fn unreachable_file_open(_: String) -> String {
    unreachable!()
}
