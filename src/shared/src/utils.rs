//! Various shared bits and pieces for the compiler.

use regex::Regex;

/// Return whether any of the regexes match.
pub fn any_match(s: &str, regexes: &[&Regex]) -> bool {
    regexes.iter().any(|r| r.is_match(s))
}

/// If `c` is a digit ('0'-'9'), return its value. Roughly equal to `c.to_digit(10).unwrap()`, but
/// this doesn't panic on bad input.
#[allow(clippy::char_lit_as_u8)]
pub fn num_val(c: char) -> u8 {
    c as u8 - '0' as u8
}

/// Return char value for octal value, for example `char_from_octal('1', '0', '1') -> 'A'.
pub fn char_from_octal(c1: char, c2: char, c3: char) -> char {
    (8 * 8 * num_val(c1) + 8 * num_val(c2) + num_val(c3)) as char
}

pub fn int_from_str(s: &str) -> i128 {
    let re = Regex::new(r"[lLuU]$").unwrap();
    let mut suffix = s.len();
    while re.is_match(&s[..suffix]) {
        suffix -= 1;
    }
    if s.starts_with("0x") {
        i128::from_str_radix(&s[2..suffix], 16)
    } else if s.starts_with('0') && s[..suffix].len() > 1 {
        i128::from_str_radix(&s[1..suffix], 8)
    } else {
        i128::from_str_radix(&s[..suffix], 10)
    }
    .unwrap_or_else(|_| panic!("Invalid number: {}", s))
}

#[test]
fn octal_values() {
    assert_eq!('a', char_from_octal('1', '4', '1'));
    assert_eq!(' ', char_from_octal('0', '4', '0'));
    assert_eq!('\u{1}', char_from_octal('0', '0', '1'));
}

#[test]
fn str_values() {
    assert_eq!(0, int_from_str("0"));
    assert_eq!(1, int_from_str("1"));
    assert_eq!(1, int_from_str("01"));
    assert_eq!(1, int_from_str("0x1"));
    assert_eq!(1, int_from_str("0x01"));

    assert_eq!(8, int_from_str("8"));
    assert_eq!(8, int_from_str("010"));
    assert_eq!(8, int_from_str("0x8"));
    assert_eq!(8, int_from_str("0x08"));

    assert_eq!(10, int_from_str("10"));
    assert_eq!(10, int_from_str("012"));
    assert_eq!(10, int_from_str("0xa"));
    assert_eq!(11, int_from_str("0xb"));
    assert_eq!(12, int_from_str("0xc"));
    assert_eq!(13, int_from_str("0xd"));
    assert_eq!(14, int_from_str("0xe"));
    assert_eq!(15, int_from_str("0xf"));
    assert_eq!(16, int_from_str("0x10"));
    assert_eq!(16, int_from_str("16"));
    assert_eq!(16, int_from_str("020"));

    assert_eq!(1, int_from_str("1u"));
    assert_eq!(1, int_from_str("1U"));
    assert_eq!(1, int_from_str("1l"));
    assert_eq!(1, int_from_str("1L"));
    assert_eq!(1, int_from_str("1uL"));
    assert_eq!(1, int_from_str("1UL"));
    assert_eq!(1, int_from_str("1lL"));
    assert_eq!(1, int_from_str("1LL"));
}
