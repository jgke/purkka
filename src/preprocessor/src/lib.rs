extern crate regex;
extern crate shared;

use std::collections::{HashMap, HashSet, VecDeque};
use std::str::CharIndices;
use std::rc::Rc;

use regex::Regex;

use shared::*;

#[derive(Debug)]
pub struct Output {
    pub macro_functions: HashMap<String, ()>,
    pub output: String
}

struct MacroContext {
    symbols: HashMap<String, Vec<MacroToken>>,
    if_stack: Vec<Vec<bool>>,
    functions: HashMap<String, ()>,
}

#[derive(Clone, Debug, PartialEq)]
enum MacroToken {
    Stringify,
    TokenPaste,
    Whitespace,
    Identifier(String),
    Other(char)
}


type ParseResult<T> = Result<T, &'static str>;

impl MacroContext {
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
                        unimplemented!(),
                        //out.push(MacroToken::Identifier(self.read_identifier(&mut Context {
                        //    filename: Rc::new("".to_string()),
                        //    file_content: "".to_string(),
                        //    span: Span::new()
                        //}, &mut iter, c).data)),
                    _ => out.push(MacroToken::Other(c))
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

    fn eval_macro(&mut self, mac: &str, iter: &mut CharIndices) -> String {
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
                        if let Some(MacroToken::Other('(')) = row.get(3) {
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

    /// Divide src into 
    fn preprocess(&mut self, src: &str) -> String {
        let iter = &mut src.char_indices();
        let mut out = String::new();
        let mut can_parse_macro = true;
        let mut inside_string = false;
        loop {
            match (inside_string, iter.next(), iter.peek()) {
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
                        out.push_str(&self.eval_macro(row, iter));
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
        match self.symbols.get(identifier) {
            Some(tokens) => self.expand_macro_vec(tokens.to_vec()),
            None => identifier.to_string()
        }
    }
}

pub fn preprocess(s: &str) -> ParseResult<String> {
    Err("Not implemented")
}
