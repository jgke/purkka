use std::collections::HashMap;
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

#[derive(Debug)]
pub struct Token {
    pub ty: TokenType,
    pub data: String,
    pub span: Span
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

#[derive(Debug, Default)]
struct CTokenizer {
    macro_context: (),
    macro_functions: HashMap<String, ()>,
    tokens: Vec<Token>
}

#[derive(Clone, Debug)]
struct Context {
    filename: Rc<String>,
    file_content: String,
    span: Span
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

        self.make_context(file.to_string(), contents)
    }

    fn make_context(&mut self, file: String, file_content: String) -> Context {
        let filename = Rc::new(file);
        let span = Span { lo: 0, hi: 0, file: filename.clone() };
        Context { filename, file_content, span }
    }

    fn parse(&mut self, file: &str) -> ParseResult<()> {
        let base_context = self.push_file(file);
        let mut iter = base_context.file_content.char_indices();

        while let Some((offset, c)) = iter.next() {
            let mut context = base_context.clone();
            context.span.lo = offset;
            context.span.hi = offset;
            let token = match c {
                '#' => self.read_macro(&mut context, &mut iter),
                _ => self.parse_token(&mut context, &mut iter, c)
            };
            self.tokens.push(token);
        }
        Ok(())
    }

    fn parse_token(&self, context: &mut Context, iter: &mut CharIndices, c: char) -> Token {
        match c {
            '0' ... '9' => self.read_number(context, iter, c),
            //'a' ... 'z' | 'A' ... 'Z' | '_' => self.read_identifier(context, iter, c),
            ' ' | '\t' | '\n' => self.read_whitespace(context, iter, c),
            //'"' => self.read_string(context, iter),
            //_ => self.match_operator(context, iter, c)
            _ => unimplemented!()
        }
    }

    fn peek_str(s: &CharIndices) -> Option<char> {
        s.as_str().chars().next()
    }

    pub fn read_whitespace(&self, context: &mut Context, iter: &mut CharIndices, c: char) -> Token {
        let content = c.to_string();
        while let Some(c) = CTokenizer::peek_str(iter) {
            match c {
                ' ' | '\t' | '\n' => {
                    context.span.hi = iter.next().unwrap().0;
                }
                _ => break
            };
        }

        Token::from_context(context, TokenType::Constant(Constant::Integer()), content)
    }

    fn read_number(&self, context: &mut Context, iter: &mut CharIndices, c: char) -> Token {
        let mut number = c.to_string();

        while let Some(c) = CTokenizer::peek_str(iter) {
            match c {
                '0' ... '9' => {
                    number.push(c);
                    context.span.hi = iter.next().unwrap().0;
                },
                _ => break
            }
        }

        Token::from_context(context, TokenType::Constant(Constant::Integer()), number)
    }

    fn read_macro(&mut self, context: &mut Context, s: &mut CharIndices) -> Token {
        //let str = s.as_str();
        //for (c_macro, m) in SIMPLE_MACROS.iter() {
        //    if str.starts_with(c_macro) {
        //        return TokenType::Macro(m);
        //    }
        //}

        //if str.starts_with("include") {
        //    for _ in 0.."include".len() {
        //        s.next();
        //    }
        //    read_whitespace(s);
        //    match s.next() {
        //        Some('<') => return TokenType::MacroInclude(MacroInclude::IncludeSystem(read_until('>', s))),
        //        Some('"') => return TokenType::MacroInclude(MacroInclude::IncludeLocal(read_until('"', s))),
        //        _ => panic!("Parse error")
        //    }
        //}

        panic!("Parse error");
    }

    /*

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
    */
}

pub fn parse(file: &str) -> ParseResult<Output> {
    let mut tokenizer = CTokenizer{ ..Default::default() };
    match tokenizer.parse(file) {
        Ok(_) => Ok(Output {tokens: tokenizer.tokens, macro_functions: tokenizer.macro_functions}),
        Err(e) => Err(e)
    }
}
