//! Multipeek from itertools (Dual licensed under MIT/Apache2) except with a single-peek by
//! default, and an optional peek_n().
//!
//! Licensed under the Apache License, Version 2.0 http://www.apache.org/licenses/LICENSE-2.0 or
//! the MIT license http://opensource.org/licenses/MIT, at your option. This file may not be
//! copied, modified, or distributed except according to those terms.
//!
//! See https://github.com/bluss/rust-itertools/blob/master/LICENSE-APACHE and
//! https://github.com/bluss/rust-itertools/blob/master/LICENSE-MIT for licensing terms.

use std::collections::VecDeque;
use std::iter::Fuse;

#[derive(Clone, Debug)]
pub struct MultiPeek<I>
where
    I: Iterator,
{
    iter: Fuse<I>,
    buf: VecDeque<I::Item>,
    index: usize,
}

/// An iterator adaptor that allows the user to peek at multiple `.next()`
/// values without advancing the base iterator.
pub fn multipeek<I>(iterable: I) -> MultiPeek<I::IntoIter>
where
    I: IntoIterator,
{
    MultiPeek {
        iter: iterable.into_iter().fuse(),
        buf: VecDeque::new(),
        index: 0,
    }
}

impl<I> MultiPeek<I>
where
    I: Iterator,
{
    /// Reset the peeking “cursor”
    pub fn reset_peek(&mut self) {
        self.index = 0;
    }
}

impl<I: Iterator> MultiPeek<I> {
    /// Peek the next item in the iterator.
    pub fn peek(&mut self) -> Option<&I::Item> {
        self.index = 0;
        self.peek_n(1).pop()
    }

    /// Peek the next n items in the iterator.
    pub fn peek_n(&mut self, n: usize) -> Vec<&I::Item> {
        self.index = 0;
        for _ in 0..n {
            self.peek_and_advance();
        }
        self.index = 0;
        self.buf.iter().take(n).collect()
    }

    pub fn peek_buf(&mut self) -> Option<&I::Item> {
        if self.index < self.buf.len() {
            Some(&self.buf[self.index])
        } else {
            match self.iter.next() {
                Some(x) => {
                    self.buf.push_back(x);
                    Some(&self.buf[self.index])
                }
                None => None,
            }
        }
    }

    pub fn peek_and_advance(&mut self) -> Option<&I::Item> {
        let ret = if self.index < self.buf.len() {
            Some(&self.buf[self.index])
        } else {
            match self.iter.next() {
                Some(x) => {
                    self.buf.push_back(x);
                    Some(&self.buf[self.index])
                }
                None => return None,
            }
        };
        self.index += 1;
        ret
    }
}

impl<I> Iterator for MultiPeek<I>
where
    I: Iterator,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        self.index = 0;
        if self.buf.is_empty() {
            self.iter.next()
        } else {
            self.buf.pop_front()
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (mut low, mut hi) = self.iter.size_hint();
        let x = self.buf.len();
        low = low.saturating_add(x);
        hi = hi.and_then(|elt| elt.checked_add(x));
        (low, hi)
    }
}

// Same size
impl<I> ExactSizeIterator for MultiPeek<I> where I: ExactSizeIterator {}

#[test]
fn buf_handling() {
    let mut iter = multipeek([1, 2, 3, 4, 5].into_iter());
    assert_eq!(**iter.peek().unwrap(), 1);
    assert_eq!(**iter.peek().unwrap(), 1);
    assert_eq!(**iter.peek_buf().unwrap(), 1);
    assert_eq!(**iter.peek_buf().unwrap(), 1);
    assert_eq!(**iter.peek_and_advance().unwrap(), 1);
    assert_eq!(**iter.peek_and_advance().unwrap(), 2);
    assert_eq!(**iter.peek_buf().unwrap(), 3);
    assert_eq!(**iter.peek_buf().unwrap(), 3);
    assert_eq!(**iter.peek().unwrap(), 1);
    assert_eq!(**iter.peek_buf().unwrap(), 1);
}
