//! Various shared bits and pieces for the compiler.

use std::str::Chars;
use std::str::CharIndices;

use regex::Regex;

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
