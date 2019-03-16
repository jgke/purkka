use std::collections::{HashMap, HashSet};

use shared::fragment::{FragmentIterator, Source};
use shared::utils::*;

use macrotoken::{MacroToken, MacroTokenType};
use tokentype::{Operator, Punctuation, OPERATORS, PUNCTUATION};
use ctoken::token::SizeofExpression;
use calculator::eval_expression;

#[derive(Debug)]
pub struct Output {
    pub macro_functions: HashMap<String, ()>,
    pub output: String,
}

#[derive(Clone, Debug)]
enum Macro {
    Text(Source, Vec<MacroToken>),
    Function(Source, Vec<String>, Vec<MacroToken>, Option<String>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum MacroType {
    //Conditional macros
    If,
    Ifdef,
    Ifndef,
    Elif,
    Else,
    Endif,

    // Source of all problems with the preprocessor
    Include,

    // Define function/object, undef
    Define,
    Undef,

    // Misc. macros
    Line,
    Pragma,
    Error,

    // common extension
    Warning,

    // Unknown macro
    Unknown
}

/// Macro context.
///
/// CB has the following signature:
/// FnMut(is_local: bool, current_file: String, opened_file: String) -> String 
pub(crate) struct MacroContext<CB>
where
    CB: FnMut(bool, String, String) -> (String, String),
{
    symbols: HashMap<String, Macro>,
    if_stack: Vec<Option<bool>>,
    get_file: CB,
}

pub type ParseResult<T> = Result<T, &'static str>;

struct MacroParseIter<'a>(
    &'a mut Vec<(MacroToken, HashSet<String>)>,
    usize,
    &'a mut FragmentIterator,
);

impl<CB> MacroContext<CB>
where
    CB: FnMut(bool, String, String) -> (String, String),
{
    pub(crate) fn new(get_file: CB) -> MacroContext<CB> {
        let mut symbols = HashMap::new();
        symbols.insert("__extension__".to_string(),
                       Macro::Text(Source::dummy(), Vec::new()));
        symbols.insert("__restrict".to_string(),
                       Macro::Text(Source::dummy(), Vec::new()));
        symbols.insert("__builtin_va_list".to_string(),
                       Macro::Text(Source::dummy(), vec![
                                   MacroToken::dummy(MacroTokenType::Identifier("va_list".to_string())
                       )]));
        MacroContext {
            symbols,
            if_stack: Vec::new(),
            get_file,
        }
    }

    fn get_iterator(&mut self, filename: &str) -> FragmentIterator {
        let content = (self.get_file)(true, ".".to_string(), filename.to_string());
        FragmentIterator::new(&content.1, &content.0)
    }

    /// Divide src into MacroTokens.
    pub(crate) fn preprocess(&mut self, filename: &str) -> (Vec<MacroToken>, FragmentIterator) {
        let mut iter = self.get_iterator(filename);
        let mut out: Vec<MacroToken> = Vec::new();
        let mut can_parse_macro = true;
        loop {
            match iter.peek() {
                Some('#') => {
                    if can_parse_macro {
                        self.read_macro(&mut iter);
                    } else {
                        panic!("Spurious #");
                    }
                }
                Some(_) => {
                    let ty = self.get_token(&mut iter, can_parse_macro);
                    can_parse_macro = ty.1;
                    if let Some(token) = ty.0 {
                        out.extend(self.maybe_expand_identifier(token, &mut iter).into_iter()
                                   .filter(|t| {
                                       if let MacroTokenType::Empty = t.ty {
                                           false
                                       } else {
                                           true
                                       }
                                   }))
                    }
                }
                None => {
                    if !iter.advance_and_reset_span() {
                        break;
                    }
                }
            }
        }

        (out, iter)
    }

    fn get_some_token(&mut self, iter: &mut FragmentIterator, parse_macro: bool) -> (MacroToken, bool) {
        let res;
        'outer: loop {
            while iter.peek().is_some() {
                let tok = self.get_token(iter, parse_macro);
                match tok.0 {
                    Some(t) => {
                        res = (t, tok.1);
                        break 'outer;
                    }
                    None => {}
                }
            }
            panic!();
        }
        res
    }

    fn get_token(
        &mut self,
        iter: &mut FragmentIterator,
        parse_macro: bool,
    ) -> (Option<MacroToken>, bool) {
        let next = iter.peek_n(2);
        let token = match next.as_ref() {
            "//" => {
                self.preprocess_get_macro_line(iter, false);
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
                Some('.') => {
                    let substr = iter.peek_n(2);
                    let mut substr_iter = substr.chars();
                    substr_iter.next();
                    let token = if let Some(c) = substr_iter.next() {
                        match c {
                            '0'...'9' => self.read_number(iter),
                            _ => self.read_other(iter),
                        }
                    } else {
                        self.read_other(iter)
                    };
                    (Some(token), false)
                }
                Some(' ') => {
                    iter.next();
                    (None, parse_macro)
                }
                Some('\t') => {
                    iter.next();
                    (None, parse_macro)
                }
                Some('\n') => {
                    iter.next();
                    (None, true)
                }
                Some(c) => {
                    let token = match c {
                        'a'...'z' | 'A'...'Z' | '_' => self.read_identifier(iter),
                        '0'...'9' => self.read_number(iter),
                        _ => self.read_other(iter),
                    };
                    (Some(token), false)
                }
                None => (None, parse_macro),
            },
        };
        token
    }

    fn preprocess_flush_until(&self, until: &str, iter: &mut FragmentIterator) {
        if !iter.iter.as_str().starts_with(until) {
            iter.collect_while_map(|c, i| if i.starts_with(until) { None } else { Some(c) });
        }
        for _ in 0..until.len() {
            match iter.next() {
                Some(_) => {}
                None => panic!("Unexpected end of file"),
            }
        }
    }

    fn preprocess_get_macro_line(&self, iter: &mut FragmentIterator, parse_comments: bool) -> (String, Source) {
        if iter.peek() == Some('\n') {
            let val = iter.next_new_span().unwrap();
            let source = iter.current_source();
            return (val.to_string(), source);
        }
        iter.collect_while_flatmap(|c, i| match c {
            '\\' => {
                i.next();
                match i.peek() {
                    Some('\n') => Some(vec![' ']),
                    Some(c) => Some(vec!['\\', c]),
                    _ => panic!("\\ at end of file"),
                }
            }
            '\n' => {
                i.next();
                None
            }
            '\t' => Some(vec![' ']),
            c => {
                if c == '/' && parse_comments {
                    let next = i.peek_n(2);
                    match next.as_ref() {
                        "//" => {
                            self.preprocess_get_macro_line(i, false);
                            None
                        }
                        "/*" => {
                            i.next(); // /
                            i.next(); // *
                            self.preprocess_flush_until("*/", i);
                            if i.peek() == Some('\n') {
                                i.next();
                                None
                            } else {
                                Some(vec![])
                            }
                        }
                        _ => Some(vec![c])
                    }
                } else {
                    Some(vec![c])
                }
            }
        })
    }

    fn read_identifier(&self, iter: &mut FragmentIterator) -> MacroToken {
        let (identifier, source) = self.read_identifier_raw(iter);
        MacroToken {
            source: source,
            ty: MacroTokenType::Identifier(identifier),
        }
    }

    fn read_identifier_raw(&self, iter: &mut FragmentIterator) -> (String, Source) {
        iter.collect_while(|c| match c {
            '0'...'9' | 'a'...'z' | 'A'...'Z' | '_' => true,
            _ => false,
        })
    }

    fn read_number(&self, iter: &mut FragmentIterator) -> MacroToken {
        let exponents = &["e+", "e-", "E+", "E-", "p+", "p-", "P+", "P-"];
        let (number, source) = iter.collect_while_flatmap(|c, i| {
            exponents
                .iter()
                .filter(|e| i.starts_with(e))
                .last()
                .map(|e| {
                    i.next();
                    Some(e.chars().collect())
                })
                .unwrap_or_else(|| match c {
                    '0'...'9' => Some(vec![c]),
                    '.' => Some(vec![c]),
                    'a'...'z' | 'A'...'Z' => Some(vec![c]),
                    _ => None,
                })
        });
        MacroToken {
            source: source,
            ty: MacroTokenType::Number(number),
        }
    }

    fn read_string(&self, iter: &mut FragmentIterator) -> MacroToken {
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
                    Some(vec![self.read_string_escape(iter, c)])
                } else {
                    panic!("Unexpected end of file");
                }
            }
            c => Some(vec![c]),
        });
        MacroToken {
            source: source,
            ty: MacroTokenType::StringLiteral(content),
        }
    }

    fn read_string_escape(&self, iter: &mut FragmentIterator, c: char) -> char {
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
                    _ => return char_from_octal('0', c, second),
                }
            }
            _ => return char_from_octal('0', '0', c),
        }
    }

    fn get_hex(&self, iter: &mut FragmentIterator) -> char {
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

    fn read_other(&self, iter: &mut FragmentIterator) -> MacroToken {
        for (punctuation, p) in PUNCTUATION.iter() {
            if iter.starts_with(punctuation) {
                iter.next_new_span();
                for _ in 1..punctuation.len() {
                    iter.next();
                }
                return MacroToken {
                    source: iter.current_source(),
                    ty: MacroTokenType::Punctuation(**p),
                };
            }
        }

        for (operator, op) in OPERATORS.iter() {
            if iter.starts_with(operator) {
                iter.next_new_span();
                for _ in 1..operator.len() {
                    iter.next();
                }
                return MacroToken {
                    source: iter.current_source(),
                    ty: MacroTokenType::Operator(**op),
                };
            }
        }

        match iter.next_new_span() {
            Some(c) => MacroToken {
                source: iter.current_source(),
                ty: MacroTokenType::Other(c),
            },
            _ => unreachable!(),
        }
    }

    fn flush_whitespace(&mut self, iter: &mut FragmentIterator) {
        loop {
            match iter.peek() {
                Some(' ') | Some('\t') => iter.next(),
                _ => break
            };
        }
    }

    /// Parse a macro (eg. #define FOO BAR) and store it for later, or act instantly in case of #if
    fn read_macro(&mut self, iter: &mut FragmentIterator) {
        let (ty, mut sub_iter, total_span) = self.get_macro_type(iter);
        match ty {
            MacroType::Define => {
                let (left, _) = self.read_identifier_raw(&mut sub_iter);
                if let Some('(') = sub_iter.peek() {
                    assert_eq!(
                        self.get_token(&mut sub_iter, false).0.map(|x| x.ty),
                        Some(MacroTokenType::Punctuation(Punctuation::OpenParen))
                    );
                    let mut args = Vec::new();
                    let mut had_comma = None;
                    let mut varargs = false;
                    let mut varargs_str: Option<String> = None;
                    while sub_iter.peek().is_some() {
                        if let (Some(mut token), _) = self.get_token(&mut sub_iter, false) {
                            match (varargs, had_comma, token.ty.clone()) {
                                (_, None, MacroTokenType::Punctuation(Punctuation::CloseParen)) =>
                                    break,
                                (_, Some(false), MacroTokenType::Punctuation(Punctuation::CloseParen)) =>
                                    break,
                                (false, None, MacroTokenType::Identifier(ident)) => {
                                    args.push(ident);
                                    had_comma = Some(false);
                                }
                                (false, Some(true), MacroTokenType::Identifier(ident)) => {
                                    args.push(ident);
                                    had_comma = Some(false);
                                }
                                (false, Some(false), MacroTokenType::Punctuation(Punctuation::Comma)) => {
                                    had_comma = Some(true);
                                }
                                (false, Some(false), MacroTokenType::Punctuation(Punctuation::Varargs)) => {
                                    varargs = true;
                                    varargs_str = Some(args.pop().unwrap());
                                }
                                (false, _, MacroTokenType::Punctuation(Punctuation::Varargs)) => {
                                    varargs = true;
                                    varargs_str = Some("__VA_ARGS__".to_string());
                                    had_comma = Some(false);
                                }
                                _ => panic!("Unexpected token: {:?}\n{}", token.ty, iter.source_to_str(&token.source)),
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

                    self.symbols
                        .insert(left, Macro::Function(total_span, args, right, varargs_str));
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
            }
            MacroType::Include => {
                loop {
                    match sub_iter.peek() {
                        Some(' ') | Some('\t') => sub_iter.next(),
                        _ => break,
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

                let (filename, _) =
                    sub_iter.collect_while(|x| (is_quote && x != '"') || (!is_quote && x != '>'));

                let end = self.read_other(&mut sub_iter);
                if let MacroTokenType::Other('"') = end.ty {
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

                let content = (self.get_file)(is_quote, iter.current_filename(), filename.clone());
                iter.split_and_push_file(&content.1, &content.0);
            }
            MacroType::Undef => {
                let (left, _) = self.read_identifier_raw(&mut sub_iter);
                self.symbols.remove(&left);
            }
            MacroType::If => self.handle_if(iter, &mut sub_iter), 
            MacroType::Elif => self.handle_elif(iter, &mut sub_iter),
            MacroType::Ifdef | MacroType::Ifndef =>
                self.handle_ifdef(iter, &mut sub_iter, ty == MacroType::Ifndef),
            MacroType::Else => self.handle_else(iter),
            MacroType::Endif => self.handle_endif(),

            ty => unimplemented!("{:?} not implemented", ty)
        }
    }

    fn get_macro_type(&mut self, iter: &mut FragmentIterator) -> (MacroType, FragmentIterator, Source) {
        let (row_string, total_span) = self.preprocess_get_macro_line(iter, true);
        let mut sub_iter = FragmentIterator::with_offset(&iter.current_filename(), &row_string, total_span.span.lo, &iter);
        assert_eq!(sub_iter.peek(), Some('#'));
        assert_eq!(
            self.get_some_token(&mut sub_iter, false).0.ty,
            MacroTokenType::Operator(Operator::Macro));
        self.flush_whitespace(&mut sub_iter);
        let (ty, _) = self.read_identifier_raw(&mut sub_iter);
        let macro_type = match ty.as_ref() {
            "if" => MacroType::If,
            "ifdef" => MacroType::Ifdef,
            "ifndef" => MacroType::Ifndef,
            "elif" => MacroType::Elif,
            "else" => MacroType::Else,
            "endif" => MacroType::Endif,
            "include" => MacroType::Include,
            "define" => MacroType::Define,
            "undef" => MacroType::Undef,
            "line" => MacroType::Line,
            "pragma" => MacroType::Pragma,
            "error" => MacroType::Error,
            "warning" => MacroType::Warning,
            _ => MacroType::Unknown
        };
        (macro_type, sub_iter, total_span)
    }

    fn evaluate_if(&mut self, sub_iter: &mut FragmentIterator) -> bool {
        let mut tokens = vec![];
        while sub_iter.peek().is_some() {
            if let Some(token) = self.get_token(sub_iter, false).0 {
                if token.get_identifier_str() == Some("defined".to_string()) {
                    let next = self.get_some_token(sub_iter, false).0;
                    let (ident, source) = if next.ty == MacroTokenType::Punctuation(Punctuation::OpenParen) {
                        let ident_token = self.get_some_token(sub_iter, false).0;
                        let tmp_ident = ident_token.get_identifier_str().unwrap().to_string();
                        let close_paren = self.get_some_token(sub_iter, false).0.ty;
                        assert!(close_paren == MacroTokenType::Punctuation(Punctuation::CloseParen));
                        (tmp_ident, ident_token.source)
                    } else {
                        let tmp_ident = next.get_identifier_str().unwrap().to_string();
                        (tmp_ident, next.source)
                    };
                    let tok = if self.symbols.contains_key(&ident) {
                        MacroToken {
                            source,
                            ty: MacroTokenType::Number("1".to_string())
                        }
                    } else {
                        MacroToken {
                            source,
                            ty: MacroTokenType::Number("0".to_string())
                        }
                    };
                    tokens.push(tok);
                } else {
                    // Undefined behaviour: defined() appearing as the result of an expansion.
                    // Resolution: pass it forward as an identifier, for simplicity. GCC resolves
                    // it 'normally'. Probably change later to follow GCC's example.
                    let expanded = self.maybe_expand_identifier(token, sub_iter);
                    tokens.extend(expanded)
                }
            }
        }
        for ref mut token in &mut tokens {
            if token.get_identifier_str().is_some() {
                token.ty = MacroTokenType::Number("0".to_string());
            }
        }

        // Actual evaluation part.
        eval_expression(&tokens)
    }

    fn handle_if(&mut self, iter: &mut FragmentIterator, sub_iter: &mut FragmentIterator) {
        let value = self.evaluate_if(sub_iter);

        self.if_stack.push(Some(value));

        if !value {
            self.skip_lines_to_else_or_endif(iter, true);
        }
    }

    fn handle_elif(&mut self, iter: &mut FragmentIterator, sub_iter: &mut FragmentIterator) {
        let popped = self.if_stack.pop();
        match popped {
            Some(Some(false)) => {
                let value = self.evaluate_if(sub_iter);
                self.if_stack.push(Some(value));

                if !value {
                    self.skip_lines_to_else_or_endif(iter, true);
                }
            }
            Some(Some(true)) => {
                self.if_stack.push(Some(true));
                self.skip_lines_to_else_or_endif(iter, true);
            }
            Some(None) => panic!("Cannot handle #elif after #else"),
            None => panic!("Spurious #else")
        }
    }

    fn handle_ifdef(&mut self, iter: &mut FragmentIterator, sub_iter: &mut FragmentIterator, is_ifndef: bool) {
        loop {
            match sub_iter.peek() {
                Some(' ') | Some('\t') => sub_iter.next(),
                _ => break,
            };
        }

        let token = self.read_identifier(sub_iter);
        let ident = token.get_identifier_str().unwrap();

        let truth_value = is_ifndef ^ (self.symbols.get(&ident).is_some());

        self.if_stack.push(Some(truth_value));

        if !truth_value {
            self.skip_lines_to_else_or_endif(iter, true);
        }
    }

    fn flush_until_pound_or_newline(&self, iter: &mut FragmentIterator) {
        'outer: loop {
            match iter.peek() {
                Some('#') | Some('\n') => return,
                Some(' ') | Some('\t') => {
                    iter.next();
                    continue 'outer;
                }
                Some(_) => {
                    loop {
                        let next = iter.peek_n(2);
                        match (next.as_ref(), iter.peek()) {
                            ("//", _) => {
                                self.preprocess_get_macro_line(iter, false);
                                continue 'outer;
                            }
                            ("/*", _) => {
                                iter.next(); // /
                                iter.next(); // *
                                self.preprocess_flush_until("*/", iter);
                                continue 'outer;
                            }
                            (_, Some('\n')) => {
                                iter.next();
                                continue 'outer;
                            }
                            (_, None) => return,
                            _ => {}
                        };
                        iter.next();
                    }
                }
                None => return,
            }
        }
    }

    fn skip_lines_to_else_or_endif(&mut self, iter: &mut FragmentIterator, accept_else: bool) {
        let mut skip_to = 0;
        while iter.peek().is_some() {
            self.flush_until_pound_or_newline(iter);
            let (next_row, line_src) = self.preprocess_get_macro_line(iter, true);
            let mut sub_iter = FragmentIterator::with_offset(&iter.current_filename(), &next_row, line_src.span.lo, &iter);
            match sub_iter.peek() {
                Some('#') => {
                    match (skip_to, accept_else, self.get_macro_type(&mut sub_iter)) {
                        (0, true, (MacroType::Elif, mut sub_iter, _)) => {
                            self.handle_elif(iter, &mut sub_iter);
                            return;
                        }
                        (0, true, (MacroType::Else, _, _)) => {
                            self.handle_else(iter);
                            return;
                        }
                        (0, false, (MacroType::Else, _, _)) => panic!(),
                        (0, _, (MacroType::Endif, _, _)) => {
                            self.handle_endif();
                            return;
                        }
                        (_, _, (MacroType::If, _, _)) | (_, _, (MacroType::Ifdef, _, _)) | (_, _, (MacroType::Ifndef, _, _)) => {
                            skip_to += 1;
                        }
                        (_, _, (MacroType::Endif, _, _)) => {
                            skip_to -= 1;
                        }
                        _ => {}
                    };
                }
                _ => {}
                //_ => {
                //    self.get_token(&mut sub_iter, false);
                //}
            };
        }
        if iter.peek().is_none() {
            panic!("Unexpected eof");
        }
    }

    fn handle_else(&mut self, iter: &mut FragmentIterator) {
        let popped = self.if_stack.pop();
        if let Some(Some(truth_value)) = popped {
            self.if_stack.push(None);
            if truth_value {
                self.skip_lines_to_else_or_endif(iter, false);
            }
        } else if let Some(None) = popped {
            panic!("Cannot handle #else after another #else")
        } else {
            panic!("Spurious #else");
        }
    }

    fn handle_endif(&mut self) {
        // line has been consumed already here, so just handle the if stack
        assert!(self.if_stack.pop().is_some());
    }

    // Macro utilities
    fn has_next_token(&self, ctx: &MacroParseIter) -> bool {
        let MacroParseIter(list, index, iter) = ctx;
        return list.len() > *index || iter.peek().is_some();
    }
    fn get_next_token(
        &mut self,
        ctx: &mut MacroParseIter,
    ) -> (Option<(MacroToken, HashSet<String>)>, bool) {
        let MacroParseIter(list, index, iter) = ctx;

        if list.len() > *index {
            return (Some(list[*index].clone()), true);
        }

        return (
            self.get_token(iter, false).0.map(|x| (x, HashSet::new())),
            false,
        );
    }
    fn get_next_token_mut(
        &mut self,
        ctx: &mut MacroParseIter,
    ) -> Option<(MacroToken, HashSet<String>)> {
        if let (Some((token, used_names)), consumed_index) = self.get_next_token(ctx) {
            if consumed_index {
                ctx.0.pop();
            }
            Some((token, used_names))
        } else if self.has_next_token(ctx) {
            self.get_next_token_mut(ctx)
        } else {
            None
        }
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
    fn maybe_expand_identifier(
        &mut self,
        token: MacroToken,
        iter: &mut FragmentIterator,
    ) -> Vec<MacroToken> {
        let mut list: Vec<(MacroToken, HashSet<String>)> = vec![(token.clone(), HashSet::new())];
        {
            let mut iter = MacroParseIter(&mut list, 0, iter);
            while iter.1 < iter.0.len() {
                self.maybe_expand_identifier_full(&mut iter);
            }
        }

        for t in &list {
            if let MacroTokenType::Identifier(ref ident) = t.0.ty {
                if ident == "sizeof" {
                    panic!();
                }
            }
        }

        let result: Vec<MacroToken> = list.into_iter().map(|(x, _)| x).collect();
        if result.len() == 0 {
            vec![MacroToken::dummy(MacroTokenType::Empty)]
        } else {
            result
        }
    }

    fn maybe_expand_identifier_full(&mut self, iter: &mut MacroParseIter) {
        let (t, used_names) = &iter.0[iter.1].clone();
        let ident_str = t.get_identifier_str();
        if ident_str
            .clone()
            .map(|ident| used_names.contains(&ident))
            .unwrap_or(true)
        {
            iter.1 += 1;
            return;
        } else {
            let maybe_syms = ident_str
                .clone()
                .and_then(|ident| self.symbols.get(&ident))
                .map(|x| x.clone());

            match maybe_syms {
                Some(Macro::Text(span, body)) => {
                    // add t to used macros
                    //
                    // remove t from token list
                    // for tt in replacement list:
                    //     add (tt, used macros) to token list
                    let mut tokens = self.expand_text_macro(
                        &ident_str.unwrap(),
                        &span,
                        &t.source,
                        &body,
                        used_names,
                        iter.2
                    );

                    let mut rest = iter.0.split_off(iter.1 + 1);
                    iter.0.remove(iter.1);
                    if tokens.len() != 0 {
                        iter.0.append(&mut tokens);
                    } else {
                        iter.0.push((
                                MacroToken::dummy(MacroTokenType::Empty),
                                HashSet::new()));
                    }
                    iter.0.append(&mut rest);
                }
                Some(Macro::Function(span, args, body, varargs)) => {
                    self.maybe_expand_function_macro(
                        iter, t, used_names, ident_str, &span, &args, &body, varargs
                    );
                }
                None => {
                    if ident_str == Some("sizeof".to_string()) {
                        self.parse_sizeof_expr(iter);
                    } else {
                        iter.1 += 1;
                    }
                    return;
                }
            }
        }
    }

    fn parse_sizeof_expr(
        &mut self,
        iter: &mut MacroParseIter) {
        // sizeof unary-expression
        // sizeof ( type-name ) 
        let start = self.get_next_token_mut(iter).map(|(t, _)| t);
        assert_eq!(
            start.as_ref().map(|t| &t.ty),
            Some(&MacroTokenType::Identifier("sizeof".to_string())));
        let mut start_span = start.unwrap().source;
        let mut ident = self.get_next_token_mut(iter).map(|(t, _)| t);
        let mut had_open_paren = false;
        if let Some(MacroTokenType::Punctuation(Punctuation::OpenParen)) = ident.as_ref().map(|t| &t.ty) {
            ident = self.get_next_token_mut(iter).map(|(t, _)| t);
            had_open_paren = true;
        }
        loop {
            if let Some(MacroTokenType::Identifier(_)) = ident.as_ref().map(|t| &t.ty) {
                break;
            }
            ident = self.get_next_token_mut(iter).map(|(t, _)| t);
            start_span.span.hi = ident.as_ref().unwrap().source.span.hi;
        }
        let mut end;
        loop {
            if !had_open_paren {
                break;
            }
            end = self.get_next_token_mut(iter).map(|(t, _)| t);
            start_span.span.hi = end.as_ref().unwrap().source.span.hi;
            println!("{}", iter.2.source_to_str(&end.as_ref().unwrap().source));
            match end.as_ref().map(|t| &t.ty) {
                Some(&MacroTokenType::Operator(Operator::Times)) => {},
                Some(&MacroTokenType::Punctuation(Punctuation::CloseParen)) => break,
                _ => panic!()
            }
        }

        let size = match &ident.map(|t| t.ty) {
            Some(MacroTokenType::Identifier(ident)) => {
                match ident.as_ref() {
                    "int" | "void" | "size_t" | "long" => MacroTokenType::Sizeof(SizeofExpression::Static(8)),
                    _ => MacroTokenType::Sizeof(SizeofExpression::Dynamic(ident.to_string()))
                }
            }
            _ => panic!()
        };

        iter.0.push((MacroToken {
            ty: size,
            source: start_span
        }, HashSet::new()));
    }

    fn maybe_expand_function_macro(
        &mut self,
        iter: &mut MacroParseIter,
        t: &MacroToken,
        used_names: &HashSet<String>,
        ident_str: Option<String>,
        span: &Source,
        args: &Vec<String>,
        body: &Vec<MacroToken>,
        varargs: Option<String>
    ) {
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
            if self.has_next_token(&MacroParseIter(iter.0, iter.1 + 1, iter.2)) {
                let (next_tok, consumed_index) =
                    self.get_next_token(&mut MacroParseIter(iter.0, iter.1 + 1, iter.2));

                match (next_tok.clone().map(|x| x.0.ty), consumed_index) {
                    (Some(MacroTokenType::Punctuation(Punctuation::OpenParen)), true) =>
                        break,
                    (Some(MacroTokenType::Punctuation(Punctuation::OpenParen)), false) => {
                        iter.0.push(next_tok.unwrap());
                        break;
                    }
                    // we consumed whitespace from the iter in this case, so just
                    // continue
                    (None, false) => continue,
                    // We got something else than whitespace or ( in these cases
                    (Some(_), false) => {
                        iter.0.push(next_tok.unwrap());
                        return;
                    }
                    (Some(_), true) => {
                        iter.1 += 1;
                        return;
                    }
                    (None, true) => unreachable!(),
                }
            } else {
                return;
            }
        }
        let (consumed_count, mut tokens) = self.expand_function_macro(
            &mut MacroParseIter(iter.0, iter.1 + 2, iter.2),
            &ident_str.unwrap(),
            &span,
            &t.source,
            &args,
            &body,
            used_names,
            varargs,
        );
        if tokens.len() == 0 {
            tokens.push((
                    MacroToken::dummy(MacroTokenType::Empty),
                    HashSet::new())
            );
        }
        iter.0
            .splice(iter.1..iter.1 + 2 + consumed_count, tokens.into_iter())
            .last();
    }

    /// Get the expanded form of ident.
    ///
    /// Algorithm part:
    ///     remove t from token list
    ///     for tt in replacement list:
    ///         add (tt, used macros) to token list
    fn expand_text_macro(
        &mut self,
        ident: &str,
        body_span: &Source,
        ident_span: &Source,
        body: &Vec<MacroToken>,
        used_names: &HashSet<String>,
        frag_iter: &FragmentIterator
    ) -> Vec<(MacroToken, HashSet<String>)> {
        let mut more_used_names = used_names.clone();
        more_used_names.insert(ident.to_string());
        let mut out = Vec::new();
        let mut iter = body.iter().peekable();
        while iter.peek().is_some() {
            let mut out_token = {
                let mut t = iter.next().unwrap().clone();
                if iter
                    .peek()
                    .map(|t| t.ty == MacroTokenType::Operator(Operator::MacroPaste))
                    == Some(true)
                {
                    iter.next(); // MacroPaste
                    let mut next = iter.next().unwrap().clone();
                    combine_tokens(&t, &t.source, &next, frag_iter)
                } else {
                    t
                }
            };
            out_token.respan_front(body_span);
            out_token.respan_front(ident_span);
            out.push((out_token, more_used_names.clone()))
        }
        out
    }

    /// Algorithm part:
    ///     add n to used macros
    ///
    ///     read function macro arguments
    ///     replace arguments in function macro body
    ///
    ///     remove t from token list
    ///     for tt in replacement list:
    ///         add (tt, used macros) to token list
    fn expand_function_macro(
        &mut self,
        iter: &mut MacroParseIter,
        ident: &str,
        body_span: &Source,
        args_span: &Source,
        fn_args: &Vec<String>,
        body: &Vec<MacroToken>,
        used_names: &HashSet<String>,
        varargs: Option<String>,
    ) -> (usize, Vec<(MacroToken, HashSet<String>)>) {
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

        let mut allow_parse_fail = HashSet::new();
        let mut force_parse = HashSet::new();

        {
            let mut i = 0;
            while i < body.len() {
                if body[i].ty == MacroTokenType::Operator(Operator::Macro) {
                    i += 1;
                    allow_parse_fail.insert(body[i].get_identifier_str().unwrap());
                } else if body[i].get_identifier_str().is_some() {
                    force_parse.insert(body[i].get_identifier_str().unwrap());
                }
                i += 1;
            }
        }

        let (consume_count, more_syms, mut total_span, has_expanded, arg_spans) =
            self.parse_function_macro_arguments(iter, args_span, fn_args,
                                                &allow_parse_fail.difference(&force_parse).collect(),
                                                varargs);
        total_span.span.source = None;
        let mut sub_iter = body.iter().peekable();
        let mut res: Vec<(MacroToken, HashSet<String>)> = Vec::new();

        while let Some(t) = sub_iter.next() {
            let outer_source = t.source.clone();
            match t.ty.clone() {
                MacroTokenType::Identifier(ident) => {
                    let syms = more_syms.get(&ident);
                    match syms {
                        Some(tt) => {
                            if tt.len() == 0 {
                                res.push((
                                        MacroToken::dummy(MacroTokenType::Empty),
                                        HashSet::new()));
                            }
                            res.append(
                                &mut tt
                                    .iter()
                                    .map(|x| {
                                        let mut nx = x.clone();
                                        if !has_expanded {
                                            nx.respan_back(&total_span);
                                        }
                                        nx.respan_back(&outer_source);
                                        (nx, used_syms.clone())
                                    })
                                    .collect(),
                            );
                        }
                        None => {
                            let mut n_sym = t.clone();
                            res.push((n_sym, used_syms.clone()))
                        }
                    }
                }
                MacroTokenType::Operator(Operator::Macro) => {
                    if let Some(ident) = sub_iter.peek().and_then(|t| t.get_identifier_str().clone()) {
                        if let Some(span) = arg_spans.get(&ident) {
                            let t_ty = MacroTokenType::StringLiteral(iter.2.top_source_to_str(span));

                            sub_iter.next(); // identifier
                            let mut t = MacroToken {
                                source: span.clone(),
                                ty: t_ty
                            };
                            res.push((t, used_syms.clone()));
                        } else {
                            panic!("Identifier {} is not an argument and cannot be stringified", ident);
                        }
                    } else {
                        panic!();
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
            if res
                .get(i + 1)
                .map(|t| t.0.ty == MacroTokenType::Operator(Operator::MacroPaste))
                == Some(true)
                // allow stuff like FOO(, ##a) -> FOO(, a, b)
                && res.get(i).map(|t| t.0.ty == MacroTokenType::Punctuation(Punctuation::Comma))
                != Some(true)
            {
                let map = res[i].1.clone();
                let mut replacement =
                    combine_tokens(&res[i].0, &res[i + 1].0.source, &res[i + 2].0, &iter.2);
                replacement.respan_back(&total_span);
                res.splice(i..i + 3, std::iter::once((replacement, map)))
                    .last();
            }

            let ref mut t = res[i];
            t.0.respan_back(&body_span);
            i += 1;
        }
        (consume_count, res)
    }

    fn parse_function_macro_arguments(
        &mut self,
        iter: &mut MacroParseIter,
        start: &Source,
        args: &Vec<String>,
        do_not_expand: &HashSet<&String>,
        varargs: Option<String>
    ) -> (usize, HashMap<String, Vec<MacroToken>>, Source, bool, HashMap<String, Source>) {
        let mut more_syms: HashMap<String, Vec<MacroToken>> = HashMap::new();
        let mut arg_sources: HashMap<String, Source> = HashMap::new();
        let mut depth = 0; // paren depth
        let mut arg_count = 0;
        let mut total_span = Some(start.clone());
        let mut consume_count = 0;
        let mut has_expanded = false;
        let arg_len = args.len();

        let has_varargs = varargs.is_some();
        let varargs_str = varargs.unwrap_or("__VA_ARGS__".to_string());
        let mut empty_iter = std::iter::once("".to_string());
        let mut looping_iter = std::iter::repeat(varargs_str.clone());
        let mut comma = None;

        let varargs_iter: &mut Iterator<Item = String> = match has_varargs {
            true => &mut looping_iter,
            false => &mut empty_iter
        }; 

        if has_varargs {
            more_syms.insert(varargs_str.clone(), vec![]);
        }

        let args_iter = args.iter().map(|t| t.clone()).chain(varargs_iter);
        'argfor: for arg in args_iter {
            let mut arg_vals = Vec::new();
            let mut arg_source: Option<Source> = None;
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
                    if arg_source.is_none() {
                        arg_source = Some(token.source.clone());
                    }
                    match (depth, token.ty.clone()) {
                        (0, MacroTokenType::Punctuation(Punctuation::CloseParen)) => {
                            if arg == varargs_str {
                                let mut args = arg_vals.drain(..).collect();
                                let mut existing_args = more_syms.remove(&arg)
                                    .map(|mut t| {
                                        if comma.is_some() {
                                            t.push(comma.unwrap());
                                        }
                                        t
                                    })
                                    .unwrap_or(vec![]);
                                existing_args.append(&mut args);
                                more_syms.insert(arg.clone(), existing_args);

                                let new_hi = arg_source.as_ref().unwrap().span.hi;

                                let source = arg_sources.remove(&arg)
                                    .map(|mut t| {
                                        t.span.hi = new_hi;
                                        t
                                    })
                                    .unwrap_or(arg_source.unwrap());
                                arg_sources.insert(arg.clone(), source);
                                arg_count += 1;
                            } else if arg != "" {
                                more_syms.insert(arg.clone(), arg_vals.drain(..).collect());
                                arg_sources.insert(arg.clone(), arg_source.unwrap());
                                arg_count += 1;
                            }
                            break 'argfor;
                        }
                        (_, MacroTokenType::Punctuation(Punctuation::CloseParen)) => {
                            if let Some(ref mut src) = arg_source {
                                src.span.hi = token.source.span.hi;
                            }
                            depth -= 1;
                            arg_vals.push(token)
                        }
                        (_, MacroTokenType::Punctuation(Punctuation::OpenParen)) => {
                            if let Some(ref mut src) = arg_source {
                                src.span.hi = token.source.span.hi;
                            }
                            depth += 1;
                            arg_vals.push(token)
                        }
                        (0, MacroTokenType::Punctuation(Punctuation::Comma)) => {
                            comma = Some(token);
                            if arg_vals.len() == 0 {
                                more_syms.insert(arg.clone(),
                                    vec![MacroToken::dummy(MacroTokenType::Empty)]
                                );
                            } else {
                                more_syms.insert(arg.clone(), arg_vals.drain(..).collect());
                            }
                            arg_sources.insert(arg.clone(), arg_source.unwrap());
                            arg_count += 1;
                            continue 'argfor;
                        }
                        (_, MacroTokenType::Identifier(_)) => {
                            if let Some(ref mut src) = arg_source {
                                src.span.hi = token.source.span.hi;
                            }
                            if consumed_index {
                                iter.1 -= 1;
                                consume_count -= 1;
                            } else {
                                iter.0.push((token, used_names));
                            }
                            let start_index = iter.1;
                            if do_not_expand.get(&arg).is_none() {
                                self.maybe_expand_identifier_full(iter);
                            } else {
                                iter.1 += 1;
                            }
                            let end_index = iter.1;
                            iter.1 = start_index;
                            let mut sliced = iter
                                .0
                                .splice(start_index..end_index, std::iter::empty())
                                .map(|x| x.0)
                                .collect();
                            arg_vals.append(&mut sliced);
                        }
                        _ => {
                            if let Some(ref mut src) = arg_source {
                                src.span.hi = token.source.span.hi;
                            }
                            arg_vals.push(token);
                        }
                    }
                }
            }
        }
        if !has_varargs && arg_count != arg_len {
            panic!(
                "Incorrect number of arguments: expected {}, got {}",
                arg_len, arg_count
            );
        } else if has_varargs && arg_count < arg_len {
            panic!(
                "Incorrect number of arguments: expected at least {}, got {}",
                arg_len, arg_count
            );
        }
        (consume_count, more_syms, total_span.unwrap(), has_expanded, arg_sources)
    }
}

fn combine_tokens(left: &MacroToken, source: &Source, right: &MacroToken, iter: &FragmentIterator) -> MacroToken {
    if left.ty == MacroTokenType::Empty {
        return right.clone();;
    } else if right.ty == MacroTokenType::Empty {
        return left.clone();
    }
    let left_str = left.get_macro_paste_str().unwrap();
    let right_str = right.get_macro_paste_str().unwrap();

    let mut total_source = source.bottom().clone();
    total_source.span.lo = left.source.bottom().span.lo;
    total_source.span.hi = right.source.bottom().span.hi;

    // This part feels like a bit of an overkill to parse a single token...
    let combined = format!("{}{}", left_str, right_str);
    let mut tmp_iter = FragmentIterator::new(&iter.current_filename(), &combined);
    let parsed_token = MacroContext {
        get_file: unreachable_file_open,
        if_stack: Vec::new(),
        symbols: HashMap::new(),
    }
    .get_token(&mut tmp_iter, false);

    assert!(tmp_iter.peek().is_none());
    assert!(parsed_token.0.is_some());

    MacroToken {
        source: total_source,
        ty: parsed_token.0.unwrap().ty,
    }
}

fn unreachable_file_open(_: bool, _: String, _: String) -> (String, String) {
    unreachable!()
}
