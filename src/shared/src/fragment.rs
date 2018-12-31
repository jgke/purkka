//! Multi-file iterator, which keeps track of the current file being iterated. See
//! [FragmentIterator](struct.FragmentIterator.html) for usage.
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

use std::str::CharIndices;
use utils::PeekableCharsExt;

/// This struct converts a &str to &'static str. Unsafe.
struct StringInterner {
    /// Hack: list of interned strings. Fragments actually point to a leaked Box<String>.
    pub strs: Vec<*mut str>
}

impl StringInterner {
    /// Convert a &str to a &'static str. The &'static str is valid as long as `free()` is not
    /// called.
    fn intern_str(&mut self, s: &str) -> &'static str {
        let boxed: Box<str> = s.to_string().into_boxed_str();
        let unsafe_str = Box::into_raw(boxed);
        self.strs.push(unsafe_str);
        unsafe {
            Box::leak(Box::from_raw(unsafe_str))
        }
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Span {
    /// Starting index of the span.
    pub lo: usize,
    /// End index of the span.
    pub hi: usize,
    /// If this exists, this span is created by macro expansion. Original location is specified in
    /// this field.
    pub source: Option<Box<Source>>
}

/// Source location for an expanded macro. This, too, can be a result of an expansion.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Source {
    /// The file where the expansion originated in.
    /// Todo: change to a interned shared reference (eg. Rc<> with interning)
    pub filename: String,
    /// The location in the file for the expansion.
    pub span: Span
}

/// Fragment of a source file. Possibly an expanded macro.
#[derive(Clone, Debug)]
pub struct Fragment {
    /// Content of this fragment. Not really 'static, actually contained in the FragmentIterator.
    content: &'static str,
    /// Offset for this Fragment's spans.
    offset: usize
}

/// An iterator over various files and their contents.
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
    iter: CharIndices<'static>,
    /// Hack: list of interned strings. Fragments actually point to a leaked Box<String>.
    interner: StringInterner
}

impl Iterator for FragmentIterator {
    type Item = char;

    /// Get next char in the current fragment.
    fn next(&mut self) -> Option<char> {
        if let Some((s, c)) = self.iter.next() {
            self.current_source.span.hi = s + self.current_fragment().offset;
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
    pub fn new(filename: &str, content: &str) -> FragmentIterator {
        let mut interner = StringInterner {
            strs: Vec::new()
        };
        let static_content = interner.intern_str(content);
        let mut fragments = Vec::new();
        fragments.push(Fragment {
            content: static_content,
            offset: 0
        });
        let iter = static_content.char_indices();
        FragmentIterator {
            fragments,
            current_fragment: 0,
            iter,
            current_source: Source {
                filename: filename.to_string(),
                span: Span { lo: 0, hi: 0, source: None }
            },
            interner,
        }
    }
    /// Get next char, resetting `current_span_start` to the char's location.
    /// Possibly advances to the next fragment, if the current fragment is empty.
    pub fn next_new_span(&mut self) -> Option<char> {
        if let Some((s, c)) = self.iter.next() {
            self.current_source.span.lo = s + self.current_fragment().offset;
            self.current_source.span.hi = s + self.current_fragment().offset;
            Some(c)
        } else {
            if self.current_fragment + 1 < self.fragments.len() {
                self.advance_fragment();

                // We want to remove a nested span here.
                let mut nested: Option<Box<Source>> = None;
                std::mem::swap(&mut self.current_source.span.source, &mut nested);
                self.current_source = *nested.unwrap();
                self.next_new_span()
            }
            else {
                None
            }
        }
    }

    /// Split the current Fragment, and push a new string (eg. #included file).
    /// The current iterator is advanced to the new fragment.
    pub fn split_and_push_file(&mut self, filename: &str, content: &str) {
        // The new frag
        let frag = Fragment {
            content: self.interner.intern_str(content),
            offset: 0
        };

        // The right side frag
        let mut rest_frag = self.current_fragment().clone();


        // XXX: We want to split this with an inclusive range, and split_at is exclusive for the
        // first half. As this practically happens at newlines, +1 should be next character, so
        // this shouldn't matter...
        let split_offset = self.current_source.span.hi+1;
        let cur_frag_content = rest_frag.content.split_at(split_offset);

        // Update the current frag's content to only include the left side
        self.current_fragment().content = cur_frag_content.0;

        // Add the middle (new) frag
        self.fragments.push(frag);

        // Add the right side (rest of original frag)
        rest_frag.content = cur_frag_content.1;
        rest_frag.offset = split_offset;

        self.fragments.push(rest_frag);
        self.advance_fragment();

        // We want to nest the span here.
        self.current_source = Source {
            filename: filename.to_string(),
            span: Span {
                lo: 0, hi: 0, source: Some(Box::new(self.current_source.clone()))
            }
        };
    }

    /// Collect a string until `f` return false. Returns the string and its span.
    pub fn collect_while(&mut self, f: impl Fn(char) -> bool) -> (String, Source) {
        self.collect_while_map(|c| if f(c) { Some(c) } else { None })
    }

    /// Collect a string from the iterator, until `f` return None. Returns the string
    /// and its span.
    pub fn collect_while_map(&mut self, f: impl Fn(char) -> Option<char>) -> (String, Source) {
        let mut content = String::new();
        while let Some(c) = self.iter.peek() {
            if let Some(c) = f(c) {
                content.push(c);
                self.iter.next();
            } else {
                break
            }
        }

        (content, self.current_source())
    }

    /// Get the current span.
    pub fn current_source(&self) -> Source {
        self.current_source.clone()
    }

    /// Get the current fragment.
    fn current_fragment(&mut self) -> &mut Fragment {
        &mut self.fragments[self.current_fragment]
    }

    /// Advance to the next fragment, but don't yet reset the span.
    fn advance_fragment(&mut self) {
        if self.current_fragment + 1 < self.fragments.len() {
            self.current_fragment += 1;
            self.iter = self.current_fragment().content.char_indices();
        }
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
