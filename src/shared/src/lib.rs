//! Various shared bits and pieces for the compiler.

extern crate regex;

use std::rc::Rc;
use std::str::Chars;
use std::str::CharIndices;

use regex::Regex;

/// A span in the currently parsed file.
#[derive(Clone, Debug)]
pub enum Span {
    /// A normal span, spanning from bytes `.0` to `.1`.
    Span(usize, usize),
    /// A span caused by an expanded macro. The source of the expansion is specified in `.2`.
    Expanded(usize, usize, Box<Source>),
}

/// Source location for an expanded macro. This, too, can be a result of an expansion.
#[derive(Clone, Debug)]
pub struct Source {
    /// The file where the expansion originated in.
    filename: Rc<String>,
    /// The location in the file for the expansion.
    span: Span
}

/// Fragment of a source file. Possibly an expanded macro.
#[derive(Clone, Debug)]
pub struct Fragment {
    /// Filename of the source file.
    filename: Rc<String>,
    /// Location in the source file for this fragment.
    span: Span,
    /// Content of this fragment.
    content: String,
}

/// Enable peeking for `Chars` or `CharIndices`.
pub trait PeekableCharsExt {
    /// Peek the next character, returning None in the case of end of string. 
    fn peek(&self) -> Option<char>;
}

impl<'a> PeekableCharsExt for Chars<'a> {
    fn peek(&self) -> Option<char> {
        self.as_str().chars().next()
    }
}

impl<'a> PeekableCharsExt for CharIndices<'a> {
    fn peek(&self) -> Option<char> {
        self.as_str().chars().next()
    }
}

/// Return whether any of the regexes match.
pub fn any_match(s: &str, regexes: &[&Regex]) -> bool {
    regexes.iter().any(|r| r.is_match(s))
}

/// If `c` is a digit ('0'-'9'), return its value. Roughly equal to `c.to_digit(10).unwrap()`, but
/// this doesn't panic on bad input.
pub fn num_val(c: char) -> u8 {
    return c as u8 - '0' as u8;
}

/// Return char value for octal value, for example `char_from_octal('1', '0', '1') -> 'A'.
pub fn char_from_octal(c1: char, c2: char, c3: char) -> char {
    return (8*8*num_val(c1) + 8*num_val(c2) + num_val(c3)) as char;
}
