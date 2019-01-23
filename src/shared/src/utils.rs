//! Various shared bits and pieces for the compiler.

use std::mem::swap;
use std::ops::Range;

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
    return (8 * 8 * num_val(c1) + 8 * num_val(c2) + num_val(c3)) as char;
}

/// Remove range `range` from `vec`, then insert `replacement` in its place. Does nothing if range
/// is empty. The removed range will be in `replacement`.
///
/// Example:
/// ```
/// use shared::utils::remove_and_replace;
///
/// let mut a = vec![1, 2, 3, 4];
/// let mut b = vec![5, 6];
///
/// remove_and_replace(&mut a, 1..3, &mut b);
/// assert_eq!(a, vec![1, 5, 6, 4]);
/// assert_eq!(b, vec![2, 3]);
///
/// remove_and_replace(&mut a, 1..2, &mut b);
/// assert_eq!(a, vec![1, 2, 3, 6, 4]);
/// assert_eq!(b, vec![5]);
///
/// remove_and_replace(&mut a, 1..0, &mut b);
/// assert_eq!(a, vec![1, 2, 3, 6, 4]);
/// assert_eq!(b, vec![5]);
/// ```
pub fn remove_and_replace<T>(
    vec: &mut Vec<T>,
    range: std::ops::Range<usize>,
    replacement: &mut Vec<T>,
) where
    T: std::fmt::Debug,
{
    println!("{:?} {:?} {:?}", range, vec, replacement);
    let start = range.start;
    if range.end > range.start {
        if range.end - range.start == replacement.len() {
            for i in range {
                println!("{}", i);
                swap(&mut replacement[i - start], &mut vec[i]);
            }
        } else {
            let mut removed = vec.split_off(range.start);
            let mut rest = removed.split_off(range.end - range.start);
            vec.append(replacement);
            vec.append(&mut rest);
            replacement.append(&mut removed);
        }
    }
}
