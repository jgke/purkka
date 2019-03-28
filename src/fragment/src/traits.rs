//! Extension traits.

use std::str::CharIndices;
use std::str::Chars;

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
