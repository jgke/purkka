use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use debug::debug::*;
use fragment::fragment::{FragmentIterator, Source};
use resolve::{FileQuery, ResolveResult};
use shared::intern::StringInterner;
use shared::utils::*;

use crate::calculator::eval_expression;
use crate::macrotoken::{MacroToken, MacroTokenType};
use crate::tokentype::{Operator, Punctuation, OPERATORS, PUNCTUATION};

#[derive(Debug)]
pub struct Output {
    pub macro_functions: HashMap<String, ()>,
    pub output: String,
}

#[derive(Clone, Debug)]
enum Macro {
    Text(Source, Vec<MacroToken>),
    Function(Source, Vec<Rc<str>>, Vec<MacroToken>, Option<Rc<str>>),
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
    Unknown,
}

/// Macro context.
///
/// CB has the following signature:
/// FnMut(is_local: bool, current_file: String, opened_file: String)
///     -> (full_path: String, file_content: String)
pub(crate) struct MacroContext<CB>
where
    CB: FnMut(FileQuery) -> ResolveResult,
{
    symbols: HashMap<Rc<str>, Macro>,
    if_stack: Vec<Option<bool>>,
    get_file: CB,
    iter: Option<FragmentIterator>,
    intern: StringInterner,
}

pub type ParseResult<T> = Result<T, &'static str>;
pub type ExpansionHistory = HashSet<Rc<str>>;

#[derive(Debug)]
struct MacroParseIter<'a>(
    &'a mut Vec<(MacroToken, ExpansionHistory)>,
    usize,
    &'a mut FragmentIterator,
);

impl<CB> MacroContext<CB>
where
    CB: FnMut(FileQuery) -> ResolveResult,
{
    pub(crate) fn new(get_file: CB) -> MacroContext<CB> {
        let mut intern = StringInterner::new();
        let mut symbols = HashMap::new();
        symbols.insert(
            intern.get_ref("__extension__"),
            Macro::Text(Source::dummy(), Vec::new()),
        );
        symbols.insert(
            intern.get_ref("__restrict"),
            Macro::Text(Source::dummy(), Vec::new()),
        );
        symbols.insert(
            intern.get_ref("__attribute__"),
            Macro::Function(
                Source::dummy(),
                vec![],
                vec![],
                Some(intern.get_ref("__va_args__")),
            ),
        );
        symbols.insert(
            intern.get_ref("__alignof__"),
            Macro::Function(
                Source::dummy(),
                vec![],
                vec![MacroToken {
                    ty: MacroTokenType::Number(intern.get_ref("1")),
                    source: Source::dummy(),
                }],
                Some(intern.get_ref("__va_args__")),
            ),
        );
        symbols.insert(
            intern.get_ref("__builtin_va_list"),
            Macro::Text(
                Source::dummy(),
                vec![MacroToken::dummy(MacroTokenType::Identifier(
                    intern.get_ref("va_list"),
                ))],
            ),
        );
        MacroContext {
            symbols,
            if_stack: Vec::new(),
            get_file,
            iter: None,
            intern,
        }
    }

    fn get_iterator(&mut self, filename: &str) -> FragmentIterator {
        let content = (self.get_file)(FileQuery::new(".", filename, true, true));
        let full_path = &content.full_path;
        let file_content = &content.h_content.unwrap_or(content.c_content);
        let mut iter = None;
        std::mem::swap(&mut iter, &mut self.iter);
        iter.map(|mut t| {
            // Reset the iterator by consuming all of the possibly remaining junk, then add the
            // file to the end
            while t.advance_and_reset_span() {}
            while t.next().is_some() {}
            t.split_and_push_file(full_path, file_content);
            t
        })
        .unwrap_or_else(|| FragmentIterator::new(full_path, file_content))
    }

    fn save_iterator(&mut self, mut iter: Option<FragmentIterator>) {
        assert!(self.iter.is_none());
        std::mem::swap(&mut iter, &mut self.iter);
    }

    pub(crate) fn add_definitions(&mut self, definitions: &[(&str, &str)]) {
        for (key, value) in definitions {
            let mut iter = FragmentIterator::new(key, value);
            let mut vals = Vec::new();
            while iter.peek().is_some() {
                let token = self.get_token(&mut iter, false);
                if let (Some(t), _) = token {
                    vals.push(MacroToken::dummy(t.ty));
                }
            }
            if vals.is_empty() {
                vals = vec![MacroToken::dummy(MacroTokenType::Empty)];
            }
            self.symbols
                .insert(self.intern.get_ref(key), Macro::Text(Source::dummy(), vals));
            assert!(iter.peek().is_none());
        }
    }

    /// Divide src into MacroTokens.
    pub(crate) fn preprocess(&mut self, filename: &str) -> (Vec<MacroToken>, FragmentIterator) {
        let mut out: Vec<MacroToken> = Vec::new();
        {
            let mut iter = self.get_iterator(filename);
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
                            out.extend(
                                self.maybe_expand_identifier(token, &mut iter)
                                    .into_iter()
                                    .filter(|t| {
                                        if let MacroTokenType::Empty = t.ty {
                                            false
                                        } else {
                                            true
                                        }
                                    }),
                            )
                        }
                    }
                    None => {
                        if !iter.advance_and_reset_span() {
                            break;
                        }
                    }
                }
            }
            self.save_iterator(Some(iter));
        }

        (out, self.iter.as_ref().unwrap().clone())
    }

    fn get_some_token(
        &mut self,
        iter: &mut FragmentIterator,
        parse_macro: bool,
    ) -> (MacroToken, bool) {
        let res;
        'outer: loop {
            while iter.peek().is_some() {
                let tok = self.get_token(iter, parse_macro);
                if let Some(t) = tok.0 {
                    res = (t, tok.1);
                    break 'outer;
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
        match next.as_ref() {
            "//" => {
                self.preprocess_get_macro_line(iter, false, true);
                (None, true)
            }
            "/*" => {
                iter.next(); // /
                iter.next(); // *
                self.preprocess_flush_until("*/", iter, false);
                (None, parse_macro)
            }
            "\\\n" => {
                iter.next(); // \
                iter.next(); // \n
                (None, parse_macro)
            }
            _ => match iter.peek() {
                Some('"') => (Some(self.read_string(iter)), false),
                Some('\'') => (Some(self.read_char(iter)), false),
                Some('.') => {
                    let substr = iter.peek_n(2);
                    let mut substr_iter = substr.chars();
                    substr_iter.next();
                    let token = if let Some(c) = substr_iter.next() {
                        match c {
                            '0'..='9' => self.read_number(iter),
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
                        'a'..='z' | 'A'..='Z' | '_' => self.read_identifier(iter),
                        '0'..='9' => self.read_number(iter),
                        _ => self.read_other(iter),
                    };
                    (Some(token), false)
                }
                None => (None, parse_macro),
            },
        }
    }

    fn preprocess_flush_until(
        &self,
        until: &str,
        iter: &mut FragmentIterator,
        preserve_last: bool,
    ) {
        while iter.peek().is_some() && !iter.as_str().starts_with(until) {
            iter.next();
        }
        for _ in 0..until.len() - 1 {
            match iter.next() {
                Some(_) => {}
                None => panic!("Unexpected end of file"),
            }
        }
        if !preserve_last {
            match iter.next() {
                Some(_) => {}
                None => panic!("Unexpected end of file"),
            }
        }
    }

    fn preprocess_get_macro_line(
        &self,
        iter: &mut FragmentIterator,
        parse_comments: bool,
        start_new_span: bool,
    ) -> (Vec<(usize, char)>, Source) {
        let lo = iter.current_source().span.lo;
        if iter.peek() == Some('\n') {
            let val = iter.next_new_span().unwrap();
            let source = iter.current_source();
            return (vec![(source.span.lo, val)], source);
        }
        let (s, mut src) = iter.collect_while_flatmap(|c, i| match c {
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
                            self.preprocess_get_macro_line(i, false, false);
                            None
                        }
                        "/*" => {
                            i.next(); // /
                            i.next(); // *
                            self.preprocess_flush_until("*/", i, true);
                            Some(vec![])
                        }
                        _ => Some(vec![c]),
                    }
                } else {
                    Some(vec![c])
                }
            }
        });
        if !start_new_span {
            src.span.lo = lo;
        }
        (s, src)
    }

    fn read_identifier(&mut self, iter: &mut FragmentIterator) -> MacroToken {
        let (identifier, source) = self.read_identifier_raw(iter);
        MacroToken {
            source,
            ty: MacroTokenType::Identifier(identifier),
        }
    }

    fn read_identifier_raw(&mut self, iter: &mut FragmentIterator) -> (Rc<str>, Source) {
        let (ident, src) = iter.collect_while(|c| match c {
            '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => true,
            _ => false,
        });
        (self.intern.get_ref(&ident), src)
    }

    fn read_number(&mut self, iter: &mut FragmentIterator) -> MacroToken {
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
                    'a'..='z' | 'A'..='Z' => Some(vec![c]),
                    '0'..='9' => Some(vec![c]),
                    '_' => Some(vec![c]),
                    '.' => Some(vec![c]),
                    _ => None,
                })
        });
        MacroToken {
            source,
            ty: MacroTokenType::Number(
                self.intern
                    .get_ref(&number.iter().map(|t| t.1).collect::<String>()),
            ),
        }
    }

    fn read_string(&mut self, iter: &mut FragmentIterator) -> MacroToken {
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
                        content.push(self.read_string_escape(iter, c));
                    } else {
                        panic!("Unexpected end of file");
                    }
                }
                Some(c) => content.push(c),
                None => panic!("Unexpected end of file"),
            }
        }

        MacroToken {
            source: iter.current_source(),
            ty: MacroTokenType::StringLiteral(self.intern.get_ref(&content)),
        }
    }

    fn read_char(&mut self, iter: &mut FragmentIterator) -> MacroToken {
        match iter.next_new_span() {
            Some('\'') => {}
            Some(t) => panic!("Unexpected character: {}", t),
            None => panic!("Unexpected end of file"),
        }
        let c = match iter.next() {
            Some('\\') => {
                if let Some(c) = iter.peek() {
                    self.read_string_escape(iter, c)
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
        MacroToken {
            source: iter.current_source(),
            ty: MacroTokenType::Char(c),
        }
    }

    fn read_string_escape(&self, iter: &mut FragmentIterator, c: char) -> char {
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
            c @ '0'..='7' => return self.get_octal(iter, c),
            'x' => return self.get_hex(iter),
            c => c,
        };
        iter.next();
        ret
    }

    fn get_octal(&self, iter: &mut FragmentIterator, c: char) -> char {
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
    fn get_hex(&self, iter: &mut FragmentIterator) -> char {
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
                _ => break,
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
                    let mut varargs_str: Option<Rc<str>> = None;
                    while sub_iter.peek().is_some() {
                        if let (Some(token), _) = self.get_token(&mut sub_iter, false) {
                            match (varargs, had_comma, token.ty.clone()) {
                                (_, None, MacroTokenType::Punctuation(Punctuation::CloseParen)) => {
                                    break
                                }
                                (
                                    _,
                                    Some(false),
                                    MacroTokenType::Punctuation(Punctuation::CloseParen),
                                ) => break,
                                (false, None, MacroTokenType::Identifier(ident)) => {
                                    args.push(ident);
                                    had_comma = Some(false);
                                }
                                (false, Some(true), MacroTokenType::Identifier(ident)) => {
                                    args.push(ident);
                                    had_comma = Some(false);
                                }
                                (
                                    false,
                                    Some(false),
                                    MacroTokenType::Punctuation(Punctuation::Comma),
                                ) => {
                                    had_comma = Some(true);
                                }
                                (
                                    false,
                                    Some(false),
                                    MacroTokenType::Punctuation(Punctuation::Varargs),
                                ) => {
                                    varargs = true;
                                    varargs_str = Some(args.pop().unwrap());
                                }
                                (false, _, MacroTokenType::Punctuation(Punctuation::Varargs)) => {
                                    varargs = true;
                                    varargs_str = Some(self.intern.get_ref("__VA_ARGS__"));
                                    had_comma = Some(false);
                                }
                                _ => panic!(
                                    "Unexpected token: {:?}\n{}",
                                    token.ty,
                                    iter.source_to_str(&token.source)
                                ),
                            }
                        }
                    }

                    let mut right = Vec::new();

                    while sub_iter.peek().is_some() {
                        if let (Some(token), _) = self.get_token(&mut sub_iter, false) {
                            right.push(token);
                        }
                    }

                    self.symbols
                        .insert(left, Macro::Function(total_span, args, right, varargs_str));
                } else {
                    let mut right = Vec::new();

                    while sub_iter.peek().is_some() {
                        if let (Some(token), _) = self.get_token(&mut sub_iter, false) {
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

                let content = (self.get_file)(FileQuery::new(
                    &iter.current_filename(),
                    &filename,
                    is_quote,
                    true,
                ));
                iter.split_and_push_file(
                    &content.full_path,
                    &content.h_content.unwrap_or(content.c_content),
                );
            }
            MacroType::Undef => {
                let (left, _) = self.read_identifier_raw(&mut sub_iter);
                self.symbols.remove(&left);
            }
            MacroType::If => self.handle_if(iter, &mut sub_iter),
            MacroType::Elif => self.handle_elif(iter, &mut sub_iter),
            MacroType::Ifdef | MacroType::Ifndef => {
                self.handle_ifdef(iter, &mut sub_iter, ty == MacroType::Ifndef)
            }
            MacroType::Else => self.handle_else(iter),
            MacroType::Endif => self.handle_endif(),
            MacroType::Error => panic!("#error: {}\n{:?}", sub_iter.as_str(), total_span),
            MacroType::Pragma => println!("Ignoring #pragma: {}", sub_iter.as_str()),

            ty => unimplemented!("{:?} not implemented", ty),
        }
    }

    fn get_macro_type(
        &mut self,
        iter: &mut FragmentIterator,
    ) -> (MacroType, FragmentIterator, Source) {
        let (row_string, total_span) = self.preprocess_get_macro_line(iter, true, true);
        let mut sub_iter = FragmentIterator::with_offset(
            &iter.current_filename(),
            row_string,
            total_span.span.lo,
            &iter,
        );
        assert_eq!(sub_iter.peek(), Some('#'));
        assert_eq!(
            self.get_some_token(&mut sub_iter, false).0.ty,
            MacroTokenType::Operator(Operator::Macro)
        );
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
            _ => MacroType::Unknown,
        };
        (macro_type, sub_iter, total_span)
    }

    fn evaluate_if(&mut self, sub_iter: &mut FragmentIterator) -> bool {
        let mut tokens = vec![];
        while sub_iter.peek().is_some() {
            if let Some(token) = self.get_token(sub_iter, false).0 {
                if token.get_identifier_str().map(|t| t.as_ref() == "defined") == Some(true) {
                    let next = self.get_some_token(sub_iter, false).0;
                    let (ident, source) =
                        if next.ty == MacroTokenType::Punctuation(Punctuation::OpenParen) {
                            let ident_token = self.get_some_token(sub_iter, false).0;
                            let tmp_ident = ident_token.get_identifier_str().unwrap();
                            let close_paren = self.get_some_token(sub_iter, false).0.ty;
                            assert!(
                                close_paren == MacroTokenType::Punctuation(Punctuation::CloseParen)
                            );
                            (tmp_ident, ident_token.source)
                        } else {
                            let tmp_ident = next.get_identifier_str().unwrap();
                            (tmp_ident, next.source)
                        };
                    let tok = if self.symbols.contains_key(&ident) {
                        MacroToken {
                            source,
                            ty: MacroTokenType::Number(self.intern.get_ref("1")),
                        }
                    } else {
                        MacroToken {
                            source,
                            ty: MacroTokenType::Number(self.intern.get_ref("0")),
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
        for token in &mut tokens {
            if token.get_identifier_str().is_some() {
                token.ty = MacroTokenType::Number(self.intern.get_ref("0"));
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
            None => panic!("Spurious #else"),
        }
    }

    fn handle_ifdef(
        &mut self,
        iter: &mut FragmentIterator,
        sub_iter: &mut FragmentIterator,
        is_ifndef: bool,
    ) {
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
                                self.preprocess_get_macro_line(iter, false, false);
                                continue 'outer;
                            }
                            ("/*", _) => {
                                iter.next(); // /
                                iter.next(); // *
                                self.preprocess_flush_until("*/", iter, false);
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
            let (next_row, line_src) = self.preprocess_get_macro_line(iter, true, true);
            let mut sub_iter = FragmentIterator::with_offset(
                &iter.current_filename(),
                next_row,
                line_src.span.lo,
                &iter,
            );
            if let Some('#') = sub_iter.peek() {
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
                    (_, _, (MacroType::If, _, _))
                    | (_, _, (MacroType::Ifdef, _, _))
                    | (_, _, (MacroType::Ifndef, _, _)) => {
                        skip_to += 1;
                    }
                    (_, _, (MacroType::Endif, _, _)) => {
                        skip_to -= 1;
                    }
                    _ => {}
                };
            }
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
        list.len() > *index || iter.peek().is_some()
    }

    fn get_next_token(
        &mut self,
        ctx: &mut MacroParseIter,
    ) -> (Option<(MacroToken, ExpansionHistory)>, bool) {
        let MacroParseIter(list, index, iter) = ctx;

        if list.len() > *index {
            (Some(list[*index].clone()), true)
        } else {
            (
                self.get_token(iter, false).0.map(|x| (x, HashSet::new())),
                false,
            )
        }
    }

    fn get_next_token_mut(
        &mut self,
        ctx: &mut MacroParseIter,
    ) -> Option<(MacroToken, ExpansionHistory)> {
        if let (Some((token, used_names)), consumed_index) = self.get_next_token(ctx) {
            if consumed_index {
                ctx.0.remove(ctx.1);
            } else {
                //ctx.0.push((token.clone(), used_names.clone()));
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
        let mut list: Vec<(MacroToken, ExpansionHistory)> = vec![(token.clone(), HashSet::new())];
        {
            let mut iter = MacroParseIter(&mut list, 0, iter);
            while iter.1 < iter.0.len() {
                self.maybe_expand_identifier_full(&mut iter);
            }
        }

        let result: Vec<MacroToken> = list.into_iter().map(|(x, _)| x).collect();
        if result.is_empty() {
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
            if_debug(DebugVal::MacroExpand, || {
                println!("Maybe expanding {:?}", &ident_str)
            });
            let maybe_syms = ident_str
                .clone()
                .and_then(|ident| self.symbols.get(&ident))
                .cloned();

            match maybe_syms {
                Some(Macro::Text(span, body)) => {
                    if_debug(DebugVal::MacroExpand, || {
                        println!("Expanding {:?} as a function", &ident_str.as_ref().unwrap())
                    });
                    // add t to used macros
                    //
                    // remove t from token list
                    // for tt in replacement list:
                    //     add (tt, used macros) to token list
                    let mut tokens = self.expand_text_macro(
                        &ident_str.as_ref().unwrap(),
                        &span,
                        &t.source,
                        &body,
                        used_names,
                        iter.2,
                    );
                    if_debug(DebugVal::MacroExpand, || {
                        println!(
                            "Expanded {:?} to {:?}",
                            &ident_str.as_ref().unwrap(),
                            tokens
                                .iter()
                                .map(|t| &t.0.ty)
                                .collect::<Vec<&MacroTokenType>>()
                        )
                    });

                    let mut rest = iter.0.split_off(iter.1 + 1);
                    iter.0.remove(iter.1);
                    if !tokens.is_empty() {
                        iter.0.append(&mut tokens);
                    } else {
                        iter.0
                            .push((MacroToken::dummy(MacroTokenType::Empty), HashSet::new()));
                    }
                    iter.0.append(&mut rest);
                }
                Some(Macro::Function(span, args, body, varargs)) => {
                    if_debug(DebugVal::MacroExpand, || {
                        println!("Expanding {} as a function", &ident_str.as_ref().unwrap())
                    });
                    self.maybe_expand_function_macro(
                        iter, t, used_names, ident_str, &span, &args, &body, varargs,
                    );
                }
                None => {
                    if_debug(DebugVal::MacroExpand, || {
                        println!("Not expanding {:?}", &ident_str)
                    });
                    iter.1 += 1;
                    return;
                }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn maybe_expand_function_macro(
        &mut self,
        iter: &mut MacroParseIter,
        t: &MacroToken,
        used_names: &ExpansionHistory,
        ident_str: Option<Rc<str>>,
        span: &Source,
        args: &[Rc<str>],
        body: &[MacroToken],
        varargs: Option<Rc<str>>,
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
                    (Some(MacroTokenType::Punctuation(Punctuation::OpenParen)), true) => break,
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
            &ident_str.as_ref().unwrap(),
            &span,
            &t.source,
            &args,
            &body,
            used_names,
            varargs,
        );
        if tokens.is_empty() {
            tokens.push((MacroToken::dummy(MacroTokenType::Empty), HashSet::new()));
        }
        if_debug(DebugVal::MacroExpand, || {
            println!(
                "Expanded {:?} to {:?}",
                &ident_str.as_ref().unwrap(),
                tokens
                    .iter()
                    .map(|t| &t.0.ty)
                    .collect::<Vec<&MacroTokenType>>()
            )
        });
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
        body: &[MacroToken],
        used_names: &ExpansionHistory,
        frag_iter: &FragmentIterator,
    ) -> Vec<(MacroToken, ExpansionHistory)> {
        let mut more_used_names = used_names.clone();
        more_used_names.insert(self.intern.get_ref(ident));
        let mut out = Vec::new();
        let mut pos = 0;
        let mut body_iter: Vec<_> = body.to_vec();
        while pos < body_iter.len() {
            let mut out_token = {
                let t = &body_iter[pos];
                pos += 1;
                if body_iter
                    .get(pos)
                    .map(|t| t.ty == MacroTokenType::Operator(Operator::MacroPaste))
                    == Some(true)
                {
                    pos += 1; // MacroPaste
                    let next = &body_iter[pos];
                    pos += 1;
                    let new_t = combine_tokens(t, &t.source, next, frag_iter);
                    body_iter.insert(pos, new_t);
                    continue;
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
    ///     add n to used macros
    ///
    ///     read function macro arguments
    ///     replace arguments in function macro body
    ///
    ///     remove t from token list
    ///     for tt in replacement list:
    ///         add (tt, used macros) to token list
    #[allow(clippy::too_many_arguments)]
    fn expand_function_macro(
        &mut self,
        iter: &mut MacroParseIter,
        ident: &str,
        body_span: &Source,
        args_span: &Source,
        fn_args: &[Rc<str>],
        body: &[MacroToken],
        used_names: &ExpansionHistory,
        varargs: Option<Rc<str>>,
    ) -> (usize, Vec<(MacroToken, ExpansionHistory)>) {
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
        used_syms.insert(self.intern.get_ref(ident));

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

        let (consume_count, more_syms, mut total_span, has_expanded, arg_spans) = self
            .parse_function_macro_arguments(
                iter,
                args_span,
                fn_args,
                &allow_parse_fail.difference(&force_parse).collect(),
                varargs.clone(),
                ident,
                body_span,
            );
        total_span.span.source = None;
        let mut sub_iter = body.iter().peekable();
        let mut res: Vec<(MacroToken, ExpansionHistory)> = Vec::new();

        while let Some(t) = sub_iter.next() {
            let outer_source = t.source.clone();
            match t.ty.clone() {
                MacroTokenType::Identifier(ident) => {
                    let syms = more_syms.get(&ident);
                    match syms {
                        Some(tt) => {
                            if tt.is_empty() {
                                res.push((
                                    MacroToken::dummy(MacroTokenType::Empty),
                                    HashSet::new(),
                                ));
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
                            let n_sym = t.clone();
                            res.push((n_sym, used_syms.clone()))
                        }
                    }
                }
                MacroTokenType::Operator(Operator::MacroPaste) => {
                    if let Some(ident) = sub_iter.peek().and_then(|t| t.get_identifier_str()) {
                        if Some(&ident) == varargs.as_ref() {
                            if res.last().map(|t| &t.0.ty)
                                == Some(&MacroTokenType::Punctuation(Punctuation::Comma))
                                && more_syms[&ident]
                                    == vec![MacroToken::dummy(MacroTokenType::Empty)]
                            {
                                res.pop();
                            }
                            continue;
                        }
                    }
                    let n_sym = t.clone();
                    res.push((n_sym, used_syms.clone()))
                }
                MacroTokenType::Operator(Operator::Macro) => {
                    if let Some(ident) =
                        sub_iter.peek().and_then(|t| t.get_identifier_str().clone())
                    {
                        if let Some(span) = arg_spans.get(&ident) {
                            let t_ty = MacroTokenType::StringLiteral(
                                self.intern.get_ref(&iter.2.top_source_to_str(span)),
                            );

                            sub_iter.next(); // identifier
                            let t = MacroToken {
                                source: span.clone(),
                                ty: t_ty,
                            };
                            res.push((t, used_syms.clone()));
                        } else {
                            panic!(
                                "Identifier {} is not an argument and cannot be stringified",
                                ident
                            );
                        }
                    } else {
                        panic!();
                    }
                }
                _ => {
                    let n_sym = t.clone();
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
                continue;
            }

            let t = &mut res[i];
            t.0.respan_back(&body_span);
            i += 1;
        }
        (consume_count, res)
    }

    #[allow(clippy::type_complexity, clippy::too_many_arguments)]
    fn parse_function_macro_arguments(
        &mut self,
        iter: &mut MacroParseIter,
        start: &Source,
        args: &[Rc<str>],
        do_not_expand: &HashSet<&Rc<str>>,
        varargs: Option<Rc<str>>,
        expanding: &str,
        expanding_src: &Source,
    ) -> (
        usize,
        HashMap<Rc<str>, Vec<MacroToken>>,
        Source,
        bool,
        HashMap<Rc<str>, Source>,
    ) {
        let mut more_syms: HashMap<Rc<str>, Vec<MacroToken>> = HashMap::new();
        let mut depth = 0; // paren depth
        let mut total_span = Some(start.clone());
        let mut consume_count = 0;
        let mut has_expanded = false;
        let arg_len = args.len();

        let mut closing_paren = None;

        let has_varargs = varargs.is_some();
        let varargs_str = varargs.unwrap_or_else(|| self.intern.get_ref("__VA_ARGS__"));
        let mut empty_iter = std::iter::empty();
        let mut looping_iter = std::iter::repeat(varargs_str.clone());

        let varargs_iter: &mut dyn Iterator<Item = Rc<str>> = if has_varargs {
            &mut looping_iter
        } else {
            &mut empty_iter
        };

        if has_varargs {
            more_syms.insert(varargs_str.clone(), vec![]);
        }

        let args_iter = args.iter().cloned().chain(varargs_iter);
        'argfor: for arg in args_iter {
            if !more_syms.contains_key(&arg) {
                more_syms.insert(arg.clone(), vec![]);
            }
            while self.has_next_token(iter) {
                if let (Some((token, used_names)), consumed_index) = self.get_next_token(iter) {
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

                    if depth == 0 && matches_token!(token.ty, Punctuation, CloseParen) {
                        closing_paren = Some(token);
                        break 'argfor;
                    } else if depth == 0
                        && arg != varargs_str
                        && matches_token!(token.ty, Punctuation, Comma)
                    {
                        if more_syms.get(&arg).unwrap().is_empty() {
                            more_syms
                                .get_mut(&arg)
                                .unwrap()
                                .push(MacroToken::dummy(MacroTokenType::Empty));
                        }
                        continue 'argfor;
                    } else {
                        match &token.ty {
                            MacroTokenType::Punctuation(Punctuation::CloseParen) => {
                                more_syms.get_mut(&arg).unwrap().push(token);
                                depth -= 1;
                            }
                            MacroTokenType::Punctuation(Punctuation::OpenParen) => {
                                more_syms.get_mut(&arg).unwrap().push(token);
                                depth += 1;
                            }
                            MacroTokenType::Identifier(_) => {
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
                                more_syms.get_mut(&arg).unwrap().append(&mut sliced);
                            }
                            _ => {
                                more_syms.get_mut(&arg).unwrap().push(token);
                            }
                        }
                    }
                }
            }
            if !self.has_next_token(iter) {
                panic!("Unexpected end of file while expanding {}", expanding);
            }
        }

        more_syms.iter_mut().for_each(|(_, vals)| {
            if vals.is_empty() {
                vals.push(MacroToken::dummy(MacroTokenType::Empty))
            }
        });

        while closing_paren.is_none() && self.has_next_token(iter) {
            closing_paren = self.get_next_token_mut(iter).map(|t| t.0);
        }

        match closing_paren {
            None => panic!("Unexpected end of file while parsing {}", expanding),
            Some(t) => {
                if !matches_token!(t.ty, Punctuation, CloseParen) {
                    panic!("Unexpected token: {:?}\n{}", t.ty, t.display(iter.2))
                }
            }
        }

        let arg_count = more_syms.len();

        if !has_varargs && arg_count != arg_len && !(arg_len == 1 && arg_count == 0) {
            panic!(
                "Incorrect number of arguments: expected {}, got {} while expanding {}, definition:\n{}\nGot:\n{}",
                arg_len, arg_count, expanding, iter.2.source_to_str(expanding_src), iter.2.source_to_str(&start)
            );
        } else if has_varargs && arg_count < arg_len {
            panic!(
                "Incorrect number of arguments: expected at least {}, got {}",
                arg_len, arg_count
            );
        }
        let arg_sources = more_syms
            .iter()
            .map(|(arg, toks)| {
                let mut src = toks[0].source.clone();
                if toks[toks.len() - 1].source.span.hi != usize::max_value() {
                    src.span.hi = toks[toks.len() - 1].source.span.hi;
                }
                (arg.clone(), src)
            })
            .collect();
        (
            consume_count,
            more_syms,
            total_span.unwrap(),
            has_expanded,
            arg_sources,
        )
    }
}

fn combine_tokens(
    left: &MacroToken,
    source: &Source,
    right: &MacroToken,
    iter: &FragmentIterator,
) -> MacroToken {
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
        iter: None,
        intern: StringInterner::new(),
    }
    .get_token(&mut tmp_iter, false);

    if tmp_iter.peek().is_some() {
        println!(
            "Failed to create a valid preprocessing token when combining '{}' and '{}'",
            left.to_src(),
            right.to_src()
        );
        println!("{}", iter.source_to_str(&left.source));
        println!("{}", iter.source_to_str(&right.source));
        panic!();
    }
    assert!(tmp_iter.peek().is_none());
    assert!(parsed_token.0.is_some());

    MacroToken {
        source: total_source,
        ty: parsed_token.0.unwrap().ty,
    }
}

fn unreachable_file_open(_: FileQuery) -> ResolveResult {
    unreachable!()
}
