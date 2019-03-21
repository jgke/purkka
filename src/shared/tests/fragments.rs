extern crate shared;

use shared::fragment::{FragmentIterator, Source, Span};

#[test]
fn fragment_simple() {
    let mut iter = FragmentIterator::new("foo.h", "foo");
    assert_eq!(iter.next(), Some('f'));
    assert_eq!(iter.next(), Some('o'));
    assert_eq!(iter.next(), Some('o'));
    assert_eq!(iter.next(), None);
}

#[test]
fn fragment_push() {
    let mut iter = FragmentIterator::new("foo.h", "foo");
    assert_eq!(iter.next(), Some('f'));
    iter.split_and_push_file("other.h", "bar");
    assert_eq!(iter.next(), Some('b'));
    assert_eq!(iter.next(), Some('a'));
    assert_eq!(iter.next(), Some('r'));
    assert_eq!(iter.next(), None);
    assert_eq!(iter.advance_and_reset_span(), true);
    assert_eq!(iter.next_new_span(), Some('o'));
    assert_eq!(iter.next(), Some('o'));
    assert_eq!(iter.next(), None);
    assert_eq!(iter.advance_and_reset_span(), false);
}

#[test]
fn fragment_spans() {
    let mut iter = FragmentIterator::new("foo.h", "foo");
    assert_eq!(iter.next(), Some('f'));
    assert_eq!(
        iter.current_source(),
        Source {
            filename: "foo.h".to_string(),
            span: Span {
                lo: 0,
                hi: 0,
                source: None
            }
        }
    );
    iter.split_and_push_file("other.h", "bar");
    assert_eq!(iter.next(), Some('b'));
    assert_eq!(iter.next(), Some('a'));
    assert_eq!(iter.next(), Some('r'));
    assert_eq!(iter.next(), None);
    assert_eq!(
        iter.current_source(),
        Source {
            filename: "other.h".to_string(),
            span: Span {
                lo: 0,
                hi: 2,
                source: None
            }
        }
    );
    assert_eq!(iter.advance_and_reset_span(), true);
    assert_eq!(iter.next_new_span(), Some('o'));
    assert_eq!(iter.next(), Some('o'));
    assert_eq!(
        iter.current_source(),
        Source {
            filename: "foo.h".to_string(),
            span: Span {
                lo: 1,
                hi: 2,
                source: None
            }
        }
    );
    assert_eq!(iter.next(), None);
}

#[test]
fn take_while() {
    let mut iter = FragmentIterator::new("foo.h", "foo bar baz");
    let (s1, _) = iter.collect_while(|x| match x {
        'a'...'z' => true,
        _ => false,
    });
    assert_eq!(s1, "foo");
    assert_eq!(iter.next(), Some(' '));
    let (s2, _) = iter.collect_while(|x| match x {
        'a'...'z' => true,
        _ => false,
    });
    assert_eq!(s2, "bar");
    assert_eq!(iter.next(), Some(' '));
    let (s3, _) = iter.collect_while(|x| match x {
        'a'...'z' => true,
        _ => false,
    });
    assert_eq!(s3, "baz");
    assert_eq!(iter.next(), None);
}

#[test]
fn take_while_mut() {
    let mut iter = FragmentIterator::new("foo.h", "foo bar baz");
    let (s1, span) = iter.collect_while_map(|x, _| match x {
        'a'...'z' => Some('A'),
        _ => None,
    });
    assert_eq!(s1, "AAA");
    assert_eq!(
        span,
        Source {
            filename: "foo.h".to_string(),
            span: Span {
                lo: 0,
                hi: 2,
                source: None
            }
        }
    );

    assert_eq!(iter.next(), Some(' '));

    let (s2, span) = iter.collect_while_flatmap(|x, iter| match x {
        'a'...'z' => {
            iter.next();
            match iter.peek() {
                Some('a'...'z') => Some(vec!['D', 'B']),
                _ => None,
            }
        }
        _ => None,
    });
    assert_eq!(s2, vec![('D', 5), ('B', 5)]);
    assert_eq!(
        span,
        Source {
            filename: "foo.h".to_string(),
            span: Span {
                lo: 4,
                hi: 6,
                source: None
            }
        }
    );

    assert_eq!(iter.next(), Some(' '));

    let (s3, _) = iter.collect_while_flatmap(|x, _| match x {
        'a'...'z' => Some(vec![]),
        _ => None,
    });
    assert_eq!(s3, vec![]);

    assert_eq!(iter.next(), None);
}
