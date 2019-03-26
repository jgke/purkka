extern crate preprocessor;
extern crate shared;
extern crate ctoken;

mod common;

use common::*;
use preprocessor::macrotoken::{MacroTokenType};

#[test]
fn ifdef_else_endif() {
    process("
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
        vec![mt(
            "foo.c",
            22, 24,
            MacroTokenType::Identifier("baz".to_string()),
        ),
        mt(
            "foo.c",
            56, 58,
            MacroTokenType::Identifier("bar".to_string()),
        )
        ],
    );
}

#[test]
fn trailing_comments() {
    process("
#ifdef FOO /*
FOO */
#endif /* FOO || BAR || BAZ
FOO */",
        vec![],
    );
    process("
#if 0
#define FOO /* Ignore */
#endif",
        vec![],
    );
}

#[test]
fn trailing_comments_true() {
    process("
#if 1
#endif /*
 */",
        vec![
        ],
    );
}

#[test]
fn if_1() {
    process("#if 1\nfoo\n#endif", vec![
            mt("foo.c", 6, 8, MacroTokenType::Identifier("foo".to_string()))
    ]);
}

#[test]
fn if_0() {
    process("#if 0\nfoo\n#endif", vec![
    ]);
}

#[test]
fn if_1_gt_2() {
    process("#if 1 > 2\nfoo\n#endif", vec![
    ]);
}

#[test]
fn if_defined() {
    process("#define foo\n#if defined ( foo )\nbar\n#endif", vec![
            mt("foo.c", 32, 34, MacroTokenType::Identifier("bar".to_string()))
    ]);
    process("#if defined foo\nfoo\n#endif", vec![
    ]);
    process("#if defined(foo)\nfoo\n#endif", vec![
    ]);
    process("#if defined ( foo )\nfoo\n#endif", vec![
    ]);
}

#[test]
fn if_nested() {
    process("
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
    vec![
    ]);
    process("
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
    vec![
            mt("foo.c", 60, 62, MacroTokenType::Identifier("bar".to_string()))
    ]);
    process("
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
    vec![
            mt("foo.c", 85, 87, MacroTokenType::Identifier("baz".to_string()))
    ]);
    process("
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
    vec![
            mt("foo.c", 86, 88, MacroTokenType::Identifier("foo".to_string()))
    ]);
    process("
#if defined FOO && defined BAR
bar

#else
baz
#endif
",
    vec![
            mt("foo.c", 43, 45, MacroTokenType::Identifier("baz".to_string()))
    ]);
    process("
#define V 46000
#if V >= 80000
#define __foo_bar(s)		__bar(s)
#else
#define __foo_bar(s)
#endif
",
    vec![
    ]);
}
