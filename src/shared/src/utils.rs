//! Various shared bits and pieces for the compiler.

use regex::Regex;

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