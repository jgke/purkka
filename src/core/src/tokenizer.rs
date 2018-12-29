use std::collections::{HashMap,HashSet,VecDeque};
use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;
use std::str::CharIndices;

use tokentype::*;

#[derive(Clone, Debug)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
    pub file: Rc<String>
}

impl Span {
    pub fn new() -> Span {
        Span {
            lo: 0,
            hi: 0,
            file: Rc::new("".to_string())
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub ty: TokenType,
    pub data: String,
    pub span: Span
}

impl Token {
    fn respan(&mut self, span: Span) {
        self.span = span;
    }
}

#[derive(Clone, Debug, PartialEq)]
enum MacroToken {
    Stringify,
    TokenPaste,
    Whitespace,
    Identifier(String),
    Other(char)
}

#[derive(Debug, Default)]
struct CTokenizer {
    macro_context: HashMap<String, Vec<MacroToken>>,
    macro_functions: HashMap<String, ()>,
    tokens: Vec<Token>
}

#[derive(Clone, Debug)]
struct Context {
    filename: Rc<String>,
    file_content: String,
    span: Span
}

impl Token {
    fn from_context(context: &Context, ty: TokenType, content: String) -> Token {
        Token {
            ty,
            data: content,
            span: context.span.clone()
        }
    }
}

impl Context {
    pub fn read(&mut self, iter: &mut CharIndices, c: char,
                f: impl Fn(char) -> bool,
                wrap: impl Fn(&str) -> TokenType) -> Token {
        self.read_mut(iter, c,
                      |x, _| if f(x) { Some(x) } else { None },
                      wrap)
    }

    pub fn read_mut(&mut self, iter: &mut CharIndices, c: char,
                    f: impl Fn(char, &mut CharIndices) -> Option<char>,
                    wrap: impl Fn(&str) -> TokenType) -> Token {
        let mut content = c.to_string();
        while let Some(c) = CTokenizer::peek_str(iter) {
            if let Some(c) = f(c, iter) {
                content.push(c);
                self.span.hi = iter.next().unwrap().0;
            } else {
                break
            }
        }

        Token::from_context(self, wrap(&content), content)
    }
}

#[derive(Debug)]
pub struct Output {
    pub macro_functions: HashMap<String, ()>,
    pub tokens: Vec<Token>
}

type ParseResult<T> = Result<T, &'static str>;

impl CTokenizer {
    fn push_file(&mut self, file: &str) -> Context {
        let mut contents = String::new();

        let mut f = File::open(file).expect("file not found");
        f.read_to_string(&mut contents)
            .expect("something went wrong reading the file");

        let processed = self.preprocess(&contents);
        println!("contents: {:?}\nprocessed: {:?}", contents, processed);
        self.make_context(file.to_string(), processed)
    }

    fn make_context(&mut self, file: String, file_content: String) -> Context {
        let filename = Rc::new(file);
        let span = Span { lo: 0, hi: 0, file: filename.clone() };
        Context { filename, file_content, span }
    }

    fn preprocess_flush_until(&self, until: &str, iter: &mut CharIndices) -> String {
        let mut out = String::new();
        loop {
            if iter.as_str().starts_with(until) {
                for _  in 0..until.len() {
                    iter.next();
                }
                out.push_str(until);
                return out
            }
            match iter.next() {
                Some((_ , c)) => out.push(c),
                None => return out
            }
        }
    }

    fn preprosess_get_macro_line(&self, iter: &mut CharIndices) -> String {
        let mut out = String::new();
        loop {
            match (iter.next(), CTokenizer::peek_str(iter)) {
                (Some((_, '\\')), Some('\n')) => {
                    iter.next();
                    iter.next();
                    out.push(' ');
                },
                (Some((_, '\n')), _) => return out,
                (Some((_, ' ')), _) => out.push(' '),
                (Some((_, '\t')), _) => out.push(' '),
                (Some((_, c)), _) => out.push(c),
                (None, _) => return out
            }
        }
    }

    fn macro_eval_flush_whitespace(&self, iter: &mut CharIndices) {
        while let Some(c) = CTokenizer::peek_str(iter) {
            match c {
                ' ' | '\t' => iter.next(),
                _ => break
            };
        }
    }

    fn tokenize_macro_row(&self, mac: &str) -> Vec<MacroToken> {
        let mut out = Vec::new();
        let mut iter = mac.char_indices();
        loop {
            match (iter.next(), CTokenizer::peek_str(&iter)) {
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
                        out.push(MacroToken::Identifier(self.read_identifier(&mut Context {
                            filename: Rc::new("".to_string()),
                            file_content: "".to_string(),
                            span: Span::new()
                        }, &mut iter, c).data)),
                    _ => out.push(MacroToken::Other(c))
                }
                (None, _) => return out
            }
        }
    }

    fn eval_macro(&mut self, mac: &str) -> String {
        let row = self.tokenize_macro_row(mac);
        println!("eval: {:?}", row);
        match row.get(0) {
            Some(MacroToken::Identifier(ident)) => {
                match row.get(1) {
                    Some(MacroToken::Whitespace) => {},
                    _ => panic!("Expected whitespace")
                };
                match ident.as_ref() {
                    "define" => {
                        let name = match row.get(2) {
                            Some(MacroToken::Identifier(ident)) => {
                                ident
                            },
                            _ => panic!("Expected identifier")
                        };
                        // no whitespace before arg list
                        if let Some(MacroToken::Other('(')) = row.get(3) {
                            panic!("Function macros not supported");
                        };
                        let mut tokens = &row[4..];
                        self.macro_context.insert(name.clone(), tokens.to_vec());
                        "\n".to_string()
                    }
                    _ => panic!("Unknown macro")
                }
            },
            _ => panic!("Parse error")
        }
    }

    fn preprocess(&mut self, src: &str) -> String {
        let iter = &mut src.char_indices();
        let mut out = String::new();
        let mut can_parse_macro = true;
        let mut inside_string = false;
        loop {
            match (inside_string, iter.next(), CTokenizer::peek_str(iter)) {
                (false, Some((_, '/')), Some('/')) => {
                    self.preprocess_flush_until("\n", iter);
                    out.push('\n');
                }
                (false, Some((_, '\\')), Some('*')) => {
                    iter.next();
                    self.preprocess_flush_until("*/", iter);
                    out.push('\n');
                }
                (true, Some((_, '\\')), Some('"')) => {
                    out.push('\\');
                    iter.next();
                    out.push('"');
                }
                (_, Some((_, '"')), _) => {
                    inside_string = !inside_string;
                    out.push('"');
                }
                (_, Some((_, ' ')), _) => out.push(' '),
                (_, Some((_, '\t')), _) => out.push('\t'),
                (_, Some((_, '\n')), _) => {
                    out.push('\n');
                    can_parse_macro = true;
                    inside_string = false;
                }
                (false, Some((_, '#')), _) => {
                    if can_parse_macro {
                        let row = &self.preprosess_get_macro_line(iter);
                        out.push_str(&self.eval_macro(row));
                        //panic!("Cannot parse :(");
                    } else {
                        out.push('#');
                    }
                }
                (_, Some((_, c)), _) => {
                    can_parse_macro = false;
                    out.push(c);
                }
                (true, None, _) => panic!("Unexpected end of input"),
                (_, None, _) => return out
            }
        }
    }

    fn expand_macro_vec(&self, tokens: Vec<MacroToken>) -> String {
        let mut out = String::new();
        println!("expand: {:?}", tokens);
        let mut iter: VecDeque<MacroToken> = tokens.into_iter().collect();
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
                (Some(MacroToken::Other(s)), _, _) => out.push(s),
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
        match self.macro_context.get(identifier) {
            Some(tokens) => self.expand_macro_vec(tokens.to_vec()),
            None => identifier.to_string()
        }
    }

    fn parse_file(&mut self, file: &str) -> ParseResult<()> {
        let context = self.push_file(file);
        let mut iter = context.file_content.char_indices();

        self.parse(&mut iter, &context, &HashSet::new())
    }

    fn parse(&mut self, iter: &mut CharIndices, base_context: &Context,
             expanded_macros: &HashSet<&str>) -> ParseResult<()> {
        while let Some((offset, c)) = iter.next() {
            let mut context = base_context.clone();
            context.span.lo = offset;
            context.span.hi = offset;
            let token = self.parse_token(&mut context, iter, c);
            match token.ty {
                TokenType::Whitespace => {},
                TokenType::Identifier() => {
                    if expanded_macros.contains(token.data.as_str()) || self.macro_context.get(&token.data).is_none() {
                        self.tokens.push(token);
                    } else {
                        let expanded = self.expand_macro(&token.data, &mut HashSet::new());
                        let mut nested_expand = expanded_macros.clone();
                        nested_expand.insert(&token.data);
                        self.parse(&mut expanded.char_indices(), base_context, &nested_expand);
                    }
                },
                _ => self.tokens.push(token)
            }
        }
        Ok(())
    }

    fn parse_token(&self, context: &mut Context, iter: &mut CharIndices, c: char) -> Token {
        match c {
            '0' ... '9' => self.read_number(context, iter, c),
            'a' ... 'z' | 'A' ... 'Z' | '_' => self.read_identifier(context, iter, c),
            ' ' | '\t' | '\n' => self.read_whitespace(context, iter, c),
            '"' => self.read_string(context, iter, c),
            _ => self.read_operator(context, iter, c),
        }
    }

    fn peek_str(s: &CharIndices) -> Option<char> {
        s.as_str().chars().next()
    }

    pub fn read_whitespace(&self, context: &mut Context, iter: &mut CharIndices, c: char) -> Token {
        context.read_mut(iter, c,
                         |c, iter| match c {
                             cc @ ' ' | cc @ '\t' | cc @ '\n' => Some(cc),
                             '\\' => {
                                 if iter.as_str().starts_with("\\\n") {
                                     iter.next();
                                     Some(' ')
                                 } else {
                                     None
                                 }
                             }
                             _ => None
                         },
                         |_| TokenType::Whitespace)
    }

    fn read_number(&self, context: &mut Context, iter: &mut CharIndices, c: char) -> Token {
        context.read(iter, c,
                     |c| match c {
                         '0' ... '9' => true,
                         _ => false
                     },
                     |_| TokenType::Constant(Constant::Integer()))
    }

    fn read_identifier(&self, context: &mut Context, iter: &mut CharIndices, c: char) -> Token {
        context.read(iter, c,
                     |c| match c {
                         '0' ... '9' | 'a' ... 'z' | 'A' ... 'Z' | '_' => true,
                         _ => false
                     },
                     |content|  {
                         for (keyword, kw) in KEYWORDS.iter() {
                             if keyword.to_string() == content {
                                 return TokenType::Keyword(kw);
                             }
                         }
                         TokenType::Identifier()
                     })
    }

    fn read_identifier_peek(&self, context: &mut Context, iter: &mut CharIndices) -> Token {
        if let Some(c) = CTokenizer::peek_str(iter) {
            iter.next();
            return self.read_identifier(context, iter, c);
        }
        panic!("Unexpected end of file");
    }

    fn get_octal(&self, iter: &mut CharIndices, c: char) -> char {
        match CTokenizer::peek_str(iter) {
            Some(second @ '0'...'7') => {
                iter.next();
                match CTokenizer::peek_str(iter) {
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

    fn get_hex(&self, iter: &mut CharIndices) -> char {
        let mut num: u8 = 0;
        while let Some(c) = CTokenizer::peek_str(iter) {
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

    fn read_string_escape(&self, iter: &mut CharIndices, c: char) -> char {
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

    fn read_string(&self, context: &mut Context, iter: &mut CharIndices, c: char) -> Token {
        let mut token = context.read_mut(
            iter, c,
            |c, iter|
            match c {
                '"' => None,
                '\n' => panic!("Missing terminating \" character"),
                '\\' => {
                    iter.next();
                    if let Some(c) = CTokenizer::peek_str(iter) {
                        Some(self.read_string_escape(iter, c))
                    } else {
                        panic!("Unexpected end of file");
                    }
                },
                c => Some(c)
            },
            |content| TokenType::StringLiteral(content[1..].to_string()));

        if let Some((pos, '"')) = iter.next() {
            token.span.hi = pos;
            token.data.push('"');
            return token;
        }

        panic!("Unexpected end of file");
    }

    fn read_operator(&self, context: &mut Context, iter: &mut CharIndices, c: char) -> Token {
        let content = c.to_string();
        for (operator, op) in OPERATORS.iter() {
            if operator.starts_with(c) && iter.as_str().starts_with(&operator[1..]) {
                for _ in 1..operator.len() {
                    // unwrap() always safe here
                    context.span.hi = iter.next().unwrap().0;
                }
                return Token::from_context(context, TokenType::Operator(op), content)
            }
        }

        for (punctuation, p) in PUNCTUATION.iter() {
            if punctuation.starts_with(c) && iter.as_str().starts_with(&punctuation[1..]) {
                for _ in 1..punctuation.len() {
                    // unwrap() always safe here
                    context.span.hi = iter.next().unwrap().0;
                }
                return Token::from_context(context, TokenType::Punctuation(p), content)
            }
        }

        panic!("Unknown character: {}", c);
    }

    fn read_until(iter: &mut CharIndices, c: char) -> String {
        let mut str = String::from("");
        while let Some((_, cc)) = iter.next() {
            if c == cc {
                break;
            }
            str.push(cc);
        }
        return str;
    }
}

fn num_val(c: char) -> u8 {
    return c as u8 - '0' as u8;
}

fn char_from_octal(c1: char, c2: char, c3: char) -> char {
    return (8*8*num_val(c1) + 8*num_val(c2) + num_val(c3)) as char;
}

pub fn parse(file: &str) -> ParseResult<Output> {
    let mut tokenizer = CTokenizer{ ..Default::default() };
    match tokenizer.parse_file(file) {
        Ok(_) => Ok(Output {
            tokens: tokenizer.tokens,
            macro_functions: tokenizer.macro_functions
        }),
        Err(e) => Err(e)
    }
}
