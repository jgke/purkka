use std::collections::{HashMap,HashSet};
use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;
use std::str::CharIndices;

use tokentype::*;
use shared::*;
use preprocessor::preprocess;

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

#[derive(Debug, Default)]
struct CTokenizer {
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
        while let Some(c) = iter.peek() {
            if let Some(c) = f(c, iter) {
                content.push(c);
                //self.span.1 = iter.next().unwrap().0;
            } else {
                break
            }
        }

        Token::from_context(self, wrap(&content), content)
    }
}

#[derive(Debug)]
pub struct Output {
    //pub macro_functions: HashMap<String, ()>,
    pub tokens: Vec<Token>
}

type ParseResult<T> = Result<T, &'static str>;

impl CTokenizer {
    fn make_context(&mut self, file_content: String) -> Context {
        let filename = Rc::new("!!!".to_string());
        let span = Span::Span(0, 0);
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
            match (iter.next(), iter.peek()) {
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
        while let Some(c) = iter.peek() {
            match c {
                ' ' | '\t' => iter.next(),
                _ => break
            };
        }
    }

    fn tokenize(&mut self, content: &str) -> ParseResult<()> {
        let context = self.make_context(content.to_string());
        let mut iter = context.file_content.char_indices();

        let result = self.parse(&mut iter, &context, &HashSet::new());

        result
    }

    fn parse(&mut self, iter: &mut CharIndices, base_context: &Context,
             expanded_macros: &HashSet<&str>) -> ParseResult<()> {
        while let Some((offset, c)) = iter.next() {
            let mut context = base_context.clone();
            context.span = Span::Span(offset, offset);
            let token = self.parse_token(&mut context, iter, c);
            match token.ty {
                TokenType::Whitespace => {},
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
        if let Some(c) = iter.peek() {
            iter.next();
            return self.read_identifier(context, iter, c);
        }
        panic!("Unexpected end of file");
    }

    fn get_octal(&self, iter: &mut CharIndices, c: char) -> char {
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

    fn get_hex(&self, iter: &mut CharIndices) -> char {
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
                    if let Some(c) = iter.peek() {
                        Some(self.read_string_escape(iter, c))
                    } else {
                        panic!("Unexpected end of file");
                    }
                },
                c => Some(c)
            },
            |content| TokenType::StringLiteral(content[1..].to_string()));

        if let Some((pos, '"')) = iter.next() {
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
                    iter.next();
                }
                return Token::from_context(context, TokenType::Operator(op), content)
            }
        }

        for (punctuation, p) in PUNCTUATION.iter() {
            if punctuation.starts_with(c) && iter.as_str().starts_with(&punctuation[1..]) {
                for _ in 1..punctuation.len() {
                    iter.next();
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

pub fn parse(file: &str) -> ParseResult<Output> {
    let mut contents = String::new();

    let mut f = File::open(file).expect("file not found");
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let processed = preprocess(&contents);

    let ok_processed = match processed {
        Ok(s) => s,
        Err(e) => return Err(e)
    };

    let mut tokenizer = CTokenizer{ ..Default::default() };
    match tokenizer.tokenize(&ok_processed) {
        Ok(_) => Ok(Output {
            tokens: tokenizer.tokens,
            //macro_functions: tokenizer.macro_functions
        }),
        Err(e) => Err(e)
    }
}
