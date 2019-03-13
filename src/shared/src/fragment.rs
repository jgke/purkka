//! Multi-file iterator, which keeps track of the current file being iterated. See
//! [`FragmentIterator`] for usage.
//!
//! [`FragmentIterator`]: struct.FragmentIterator.html
//!
//! For proper error handling in C, due to `#include` and friends, one needs to keep track of the
//! origin for various identifiers.
//!
//! ```C
//! /* foo.h */
//! #define FOO "foo"
//!
//! /* foo.c */
//! #include "foo.h"
//! int main() {
//!     // expands to int foo = "foo";
//!     int foo = FOO;
//! }
//! ```
//!
//! Here, the user should be shown an diagnostic message about the origin of the error, something
//! along the following lines:
//!
//! ```text
//! Warning: Implicit pointer cast to integer (foo.c:4:15)
//!      int foo = FOO;
//!                ^^^
//! Expanded from macro 'FOO' (foo.h:1:13)
//!      #define FOO "foo"
//!                  ^^^^^
//! ```
//!
//! This module contains utilities to help tracking the spans and files.

use std::cmp::min;
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::str::CharIndices;
use traits::PeekableCharsExt;

/// This struct converts a &str to &'static str. Unsafe.
struct StringInterner {
    /// Hack: list of interned strings. Fragments actually point to a leaked Box<String>.
    pub strs: Vec<*mut str>,
}

impl StringInterner {
    /// Convert a &str to a &'static str. The &'static str is valid as long as `free()` is not
    /// called.
    fn intern_str(&mut self, s: &str) -> &'static str {
        let boxed: Box<str> = s.to_string().into_boxed_str();
        let unsafe_str = Box::into_raw(boxed);
        self.strs.push(unsafe_str);
        unsafe { Box::leak(Box::from_raw(unsafe_str)) }
    }

    /// Drop all references. This COULD be implemented as `impl Drop for StringInterner`, but I
    /// think it's better to be more explicit here, as accidental drops mean undefined behaviour.
    fn free(&mut self) {
        for s in self.strs.drain(..) {
            let p: *mut str = s;
            unsafe {
                Box::from_raw(p);
            }
        }
    }
}

/// A span in the currently parsed file.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    /// Starting index of the span.
    pub lo: usize,
    /// End index of the span.
    pub hi: usize,
    /// If this exists, this span is created by macro expansion. Original location is specified in
    /// this field.
    pub source: Option<Box<Source>>,
}

/// Source location for an expanded macro. This, too, can be a result of an expansion.

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Source {
    /// The file where the expansion originated in.
    /// Todo: change to a interned shared reference (eg. Rc<> with interning)
    pub filename: String,
    /// The location in the file for the expansion.
    pub span: Span,
}

impl Source {
    /// Add `other` to the end of the expansions
    pub fn merge(&mut self, other: &Source) {
        match self.span.source {
            None => self.span.source = Some(Box::new(other.clone())),
            Some(ref mut s) => s.merge(other),
        }
    }

    /// Get the bottom cause of this source.
    pub fn bottom(&self) -> &Source {
        match self.span.source {
            None => self,
            Some(ref s) => s.bottom(),
        }
    }

    /// Provides an iterator.
    pub fn iter(&self) -> SourceIterator<'_> {
        SourceIterator {
            next: Some(self)
        }
    }
}

pub struct SourceIterator<'a> {
    next: Option<&'a Source>
}

impl<'a> Iterator for SourceIterator<'a> {
    type Item = &'a Source;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            self.next = node.span.source
                .as_ref()
                .map(|source| &**source);
            node
        })
    }
}

/// Fragment of a source file. Possibly an expanded macro.
#[derive(Clone, Debug)]
struct Fragment {
    /// Content of this fragment. Not really 'static, actually contained in the FragmentIterator.
    content: &'static str,
    /// Offset for this Fragment's spans.
    offset: usize,
}

/// An iterator over multiple filenames and strings, keeping track of origins for each substring.
///
/// # Examples
///
/// Basic usage:
/// ```
/// use shared::fragment::FragmentIterator;
///
/// let mut iter = FragmentIterator::new("foo.h", "foo BAR baz");
/// let (identifier, _) = iter.collect_while(|x| match x {
///     'a' ...  'z' => true,
///     _ => false
/// });
/// assert_eq!(&identifier, "foo");
/// ```
pub struct FragmentIterator {
    /// The contained fragments.
    fragments: Vec<Fragment>,
    /// Current fragment index being iterated.
    /// Next fragment can be selected with next_maybe_new_fragment().
    current_fragment: usize,
    /// Current source.
    /// Can be reset with next_new_span().
    current_source: Source,
    /// Iterator for the current fragments.
    pub iter: CharIndices<'static>,
    /// All inserted files
    contents: HashMap<String, String>,
    /// Hack: list of interned strings. Fragments actually point to a leaked Box<String>.
    interner: StringInterner,
    /// Debug output is enabled (env DEBUG_FRAGMENT is defined)
    debug: bool,
}

impl Iterator for FragmentIterator {
    type Item = char;

    /// Get next char in the current fragment.
    fn next(&mut self) -> Option<char> {
        if let Some((s, c)) = self.iter.next() {
            let hi = s + self.current_fragment().offset;
            self.current_source.span.hi = hi;
            if(self.debug) {
                dbg!(self.current_source());
                dbg!(c);
            }
            Some(c)
        } else {
            None
        }
    }
}

impl Drop for FragmentIterator {
    fn drop(&mut self) {
        self.interner.free();
    }
}

impl FragmentIterator {
    /// Initialize the iterator.
    pub fn new(filename: &str, content: &str) -> FragmentIterator {
        FragmentIterator::_with_offset(filename, content, 0, HashMap::new())
    }

    /// Initialize the iterator with a predetermined offset.
    pub fn with_offset(filename: &str, content: &str, offset: usize,
                       parent: &FragmentIterator) -> FragmentIterator {
        FragmentIterator::_with_offset(filename, content, offset, parent.contents.clone())
    }

    /// Initialize the iterator with a predetermined offset.
    fn _with_offset(filename: &str, content: &str, offset: usize,
                       mut contents: HashMap<String, String>) -> FragmentIterator {
        let mut interner = StringInterner { strs: Vec::new() };
        let static_content = interner.intern_str(content);
        let mut fragments = Vec::new();
        fragments.push(Fragment {
            content: static_content,
            offset: offset,
        });
        if !contents.contains_key(filename) {
            contents.insert(filename.to_string(), content.to_string());
        }
        let iter = static_content.char_indices();
        let debug = env::var("DEBUG_FRAGMENT").is_ok();
        FragmentIterator {
            fragments,
            current_fragment: 0,
            iter,
            contents,
            current_source: Source {
                filename: filename.to_string(),
                span: Span {
                    lo: offset,
                    hi: offset,
                    source: None,
                },
            },
            interner,
            debug
        }
    }

    /// Get next char, resetting the current span to the char's location.
    /// Does not advance to the next fragment, even if the current fragment is empty.
    pub fn next_new_span(&mut self) -> Option<char> {
        if let Some((s, c)) = self.iter.next() {
            let pos = s + self.current_fragment().offset;
            self.current_source.span.lo = pos;
            self.current_source.span.hi = pos;
            Some(c)
        } else {
            None
        }
    }

    /// Split the current Fragment, and push a new string (eg. #included file).
    /// The current iterator is advanced to the new fragment.
    pub fn split_and_push_file(&mut self, filename: &str, content: &str) {
        // The new frag
        let frag = Fragment {
            content: self.interner.intern_str(content),
            offset: 0,
        };

        // The right side frag
        let mut rest_frag = self.current_fragment().clone();

        // XXX: We want to split this with an inclusive range, and split_at is exclusive for the
        // first half. As this practically happens at newlines, +1 should be next character, so
        // this shouldn't matter...
        let split_offset = self.current_source.span.hi + 1 - self.current_fragment().offset;
        let cur_frag_content = rest_frag.content.split_at(split_offset);

        // Update the current frag's content to only include the left side
        self.current_fragment().content = cur_frag_content.0;

        // Add the middle (new) frag
        self.fragments.insert(self.current_fragment + 1, frag);

        // Add the right side (rest of original frag)
        rest_frag.content = cur_frag_content.1;
        rest_frag.offset = split_offset;

        self.fragments.insert(self.current_fragment + 2, rest_frag);
        self.advance_fragment();

        let old_source = self.current_source.clone();

        // We want to nest the span here.
        self.current_source = Source {
            filename: filename.to_string(),
            span: Span {
                lo: 0,
                hi: 0,
                source: Some(Box::new(old_source)),
            },
        };

        // Insert the file to the content table, if not present.
        if self.contents.get(filename).is_none() {
            self.contents
                .insert(filename.to_string(), content.to_string());
        }
    }

    /// Collect a string until `f` return false. Returns the string and its span.
    /// See [`collect_while_map`] for semantic details.
    ///
    /// [`collect_while_map`]: #method.collect_while_map
    ///
    /// # Example
    /// ```
    /// # use shared::fragment::FragmentIterator;
    /// let mut iter = FragmentIterator::new("foo.h", "foo bar baz");
    /// let (s1, _) = iter.collect_while(|x| match x {
    ///     'a'...'z' => true,
    ///     _ => false
    /// });
    /// assert_eq!(s1, "foo");
    /// ```
    pub fn collect_while(&mut self, mut f: impl FnMut(char) -> bool) -> (String, Source) {
        self.collect_while_map(|c, _| if f(c) { Some(c) } else { None })
    }

    /// Iterate over self, map the results with f and collect to a string from the iterator. Stops
    /// when `f` return None or current fragment is empty. This will always consume at least one
    /// character from the iterator, which is stored in the string if `f` returns Some. Returns the
    /// resulting string and its span.
    ///
    /// # Example
    /// ```
    /// # use shared::fragment::FragmentIterator;
    /// let mut iter = FragmentIterator::new("foo.h", "foo bar baz");
    /// let (s1, _) = iter.collect_while_map(|x, _| match x {
    ///     'a'...'z' => Some(x.to_ascii_uppercase()),
    ///     _ => None
    /// });
    /// assert_eq!(s1, "FOO");
    /// ```
    pub fn collect_while_map(
        &mut self,
        mut f: impl FnMut(char, &mut Self) -> Option<char>,
    ) -> (String, Source) {
        let mut content = String::new();
        if let Some(c) = self.next_new_span() {
            if let Some(c) = f(c, self) {
                content.push(c);
            }
        }
        while let Some(c) = self.peek() {
            if let Some(c) = f(c, self) {
                content.push(c);
                self.next();
            } else {
                break;
            }
        }

        (content, self.current_source())
    }

    /// Iterate over self, flatmap the results with f and collect to a string from the iterator.
    /// Stops when `f` return None or current fragment is empty.
    /// See [`collect_while_map`] for semantic details.
    ///
    /// [`collect_while_map`]: #method.collect_while_map
    ///
    /// # Example
    /// ```
    /// # use shared::fragment::FragmentIterator;
    /// let mut iter = FragmentIterator::new("foo.h", "foo bar baz");
    /// let (s1, _) = iter.collect_while_flatmap(|x, _| match x {
    ///     'a'...'z' => Some(vec![x.to_ascii_uppercase(), 'a']),
    ///     _ => None
    /// });
    /// assert_eq!(s1, "FaOaOa");
    /// ```
    pub fn collect_while_flatmap(
        &mut self,
        mut f: impl FnMut(char, &mut Self) -> Option<Vec<char>>,
    ) -> (String, Source) {
        let mut content = String::new();
        if let Some(c) = self.next_new_span() {
            if let Some(chars) = f(c, self) {
                chars.into_iter().for_each(|c| content.push(c));
            }
        }
        while let Some(c) = self.peek() {
            if let Some(chars) = f(c, self) {
                chars.into_iter().for_each(|c| content.push(c));
                self.next();
            } else {
                break;
            }
        }

        (content, self.current_source())
    }

    /// Peek the next character in the current fragment.
    pub fn peek(&self) -> Option<char> {
        return self.iter.peek();
    }

    /// Peek the next character in the current fragment.
    pub fn peek_n(&self, n: usize) -> String {
        let s = self.iter.as_str();
        return s[0..min(s.len(), n)].to_string();
    }

    /// Returns whether the current fragment starts with `s`.
    pub fn starts_with(&self, s: &str) -> bool {
        return self.iter.as_str().starts_with(s);
    }

    /// Get the current span.
    pub fn current_source(&self) -> Source {
        let mut out = self.current_source.clone();
        out.span.source = None;
        out
    }

    /// Get the current span with history.
    pub fn full_current_source(&self) -> Source {
        self.current_source.clone()
    }

    /// Get the current filename.
    pub fn current_filename(&self) -> String {
        self.current_source.filename.clone()
    }

    /// Get the current fragment.
    fn current_fragment(&mut self) -> &mut Fragment {
        &mut self.fragments[self.current_fragment]
    }

    /// Advance to the next fragment, but don't yet reset the span. Returns true on success.
    fn advance_fragment(&mut self) -> bool {
        if self.current_fragment + 1 < self.fragments.len() {
            self.current_fragment += 1;
            self.iter = self.current_fragment().content.char_indices();
            true
        } else {
            false
        }
    }

    /// Advance to the next fragment and reset the span. Returns true on success.
    pub fn advance_and_reset_span(&mut self) -> bool {
        if self.current_fragment + 1 < self.fragments.len() {
            self.current_fragment += 1;
            self.iter = self.current_fragment().content.char_indices();
            let mut nested: Option<Box<Source>> = None;
            std::mem::swap(&mut self.current_source.span.source, &mut nested);
            self.current_source = *nested.unwrap();
            true
        } else {
            false
        }
    }

    /// Get the user-readable source string for a source recursively. This panics frequently if
    /// given sources from a different iterator, or if self is made using `with_offset`.
    pub fn source_to_str(&self, source: &Source) -> String {
        let mut lines: Vec<(String, (&str, usize, usize))> = Vec::new();
        let mut out = "".to_string();
        let s = &self.contents[&source.filename];
        out.push_str(s[source.span.lo..=source.span.hi].trim());
        lines.push((
            format!("{}", s[source.span.lo..=source.span.hi].trim()),
            (source.filename.as_ref(), source.span.lo, source.span.hi),
        ));
        let mut current_source = &source.span.source;
        while let Some(src) = current_source {
            let s = &self.contents[&src.filename];
            lines.push((
                format!("Expanded from: {:?}", s[src.span.lo..=src.span.hi].trim()),
                (src.filename.as_ref(), src.span.lo, src.span.hi),
            ));
            current_source = &src.span.source;
        }
        let max = lines
            .iter()
            .fold(0, |prev, (s, _)| std::cmp::max(prev, s.len()));
        lines
            .iter()
            .map(|(s, (file, lo, hi))| {
                format!("{:width$}({}: {}-{})\n", s, file, lo, hi, width = max + 1)
            })
            .collect::<Vec<String>>()
            .concat()
        //out.push('\n');
        //out
    }

    /// Get the topmost source as a plain string without filename
    pub fn top_source_to_str(&self, source: &Source) -> String {
        let s = &self.contents[&source.filename];
        s[source.span.lo..=source.span.hi].trim().to_string()
    }

    /// Get the current content
    pub fn get_current_content(&self) -> String {
        let s = &self.contents[&self.current_source().filename];
        s.to_string()
    }
}

impl fmt::Debug for FragmentIterator {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("FragmentIterator")
            .field("fragments", &self.fragments)
            .field("current_fragment", &self.current_fragment)
            .field("current_source", &self.current_source)
            .field("iter", &self.iter.as_str())
            .field("contents", &self.contents)
            .field("debug", &self.debug)
            .finish()
    }
}

#[test]
fn interner_one_string() {
    let mut intern = StringInterner { strs: Vec::new() };

    let reference = intern.intern_str("foo");
    assert_eq!(reference, "foo");
    intern.free();
}

#[test]
fn interner_more_strings() {
    let mut intern = StringInterner { strs: Vec::new() };

    intern.intern_str("A relatively long string");
    intern.intern_str("more strings");
    intern.intern_str("and more");
    intern.free();
}
