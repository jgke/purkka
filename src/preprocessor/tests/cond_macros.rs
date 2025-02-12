extern crate ctoken;
extern crate preprocessor;
extern crate shared;

mod common;

use common::*;

#[test]
fn ifdef_else_endif() {
    process(
        "
#ifdef FOO
bar
#else
baz
#endif
#define FOO
#ifdef FOO
bar
#else
baz
#endif",
        vec![
            mt("foo.c", 22, 24, ident("baz")),
            mt("foo.c", 56, 58, ident("bar")),
        ],
    );
}

#[test]
fn trailing_comments() {
    process(
        "
#ifdef FOO /*
FOO */
#endif /* FOO || BAR || BAZ
FOO */",
        vec![],
    );
    process(
        "
#if 0
#define FOO /* Ignore */
#endif",
        vec![],
    );
}

#[test]
fn trailing_comments_true() {
    process(
        "
#if 1
#endif /*
 */",
        vec![],
    );
}

#[test]
fn if_1() {
    process("#if 1\nfoo\n#endif", vec![mt("foo.c", 6, 8, ident("foo"))]);
}

#[test]
fn if_0() {
    process("#if 0\nfoo\n#endif", vec![]);
}

#[test]
fn if_1_gt_2() {
    process("#if 1 > 2\nfoo\n#endif", vec![]);
}

#[test]
fn hex_values() {
    process(
        "#if 0xf == 15\nbar\n#elif 0xff != 255\nfoo\n#endif",
        vec![mt("foo.c", 15, 17, ident("bar"))],
    );
}

#[test]
fn if_defined() {
    process(
        "#define foo\n#if defined ( foo )\nbar\n#endif",
        vec![mt("foo.c", 32, 34, ident("bar"))],
    );
    process("#if defined foo\nfoo\n#endif", vec![]);
    process("#if defined(foo)\nfoo\n#endif", vec![]);
    process("#if defined ( foo )\nfoo\n#endif", vec![]);
}

#[test]
fn if_nested() {
    process(
        "
#if defined FOO
# if defined BAR
  bar
# elif defined BAZ
  baz
# else
  foo
# endif
#endif
",
        vec![],
    );
    process(
        "
#define FOO
#define BAR
#if defined FOO
# if defined BAR
  bar
# elif defined BAZ
  baz
# else
  foo
# endif
#endif
",
        vec![mt("foo.c", 60, 62, ident("bar"))],
    );
    process(
        "
#define FOO
#define BAZ
#if defined FOO
# if defined BAR
  bar
# elif defined BAZ
  baz
# else
  foo
# endif
#endif
",
        vec![mt("foo.c", 85, 87, ident("baz"))],
    );
    process(
        "
#define FOO
#if defined FOO
# if defined BAR
  bar
# elif defined BAZ
  baz
# else
  foo
# endif
#endif
",
        vec![mt("foo.c", 86, 88, ident("foo"))],
    );
    process(
        "
#if defined FOO && defined BAR
bar

#else
baz
#endif
",
        vec![mt("foo.c", 43, 45, ident("baz"))],
    );
    process(
        "
#define V 46000
#if V >= 80000
#define __foo_bar(s)		__bar(s)
#else
#define __foo_bar(s)
#endif
",
        vec![],
    );
}
