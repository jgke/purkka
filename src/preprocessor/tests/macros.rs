extern crate preprocessor;
extern crate shared;
extern crate ctoken;

mod common;

use common::*;
use ctoken::token::SizeofExpression;
use preprocessor::macrotoken::{MacroTokenType};
use preprocessor::tokentype::{Operator, Punctuation};

#[test]
fn one_identifier() {
    process(
        "Foo",
        vec![mt(
            "foo.c",
            0,
            2,
            MacroTokenType::Identifier("Foo".to_string()),
        )],
    );
}

#[test]
fn three_identifiers() {
    process(
        "Foo Bar Baz",
        vec![
            mt("foo.c", 0, 2, MacroTokenType::Identifier("Foo".to_string())),
            mt("foo.c", 4, 6, MacroTokenType::Identifier("Bar".to_string())),
            mt(
                "foo.c",
                8,
                10,
                MacroTokenType::Identifier("Baz".to_string()),
            ),
        ],
    );
}

#[test]
fn strings() {
    process(
        "\"foo\" \"bar\\nbaz\"",
        vec![
            mt(
                "foo.c",
                0,
                4,
                MacroTokenType::StringLiteral("foo".to_string()),
            ),
            mt(
                "foo.c",
                6,
                15,
                MacroTokenType::StringLiteral("bar\nbaz".to_string()),
            ),
        ],
    );
}

#[test]
fn numbers() {
    process(
        "1 .54e+1..a5 4%",
        vec![
            mt("foo.c", 0, 0, MacroTokenType::Number("1".to_string())),
            mt(
                "foo.c",
                2,
                11,
                MacroTokenType::Number(".54e+1..a5".to_string()),
            ),
            mt("foo.c", 13, 13, MacroTokenType::Number("4".to_string())),
            mt("foo.c", 14, 14, MacroTokenType::Operator(Operator::Mod)),
        ],
    );
}

#[test]
fn operators() {
    process(
        "++ -- +++++",
        vec![
            mt("foo.c", 0, 1, MacroTokenType::Operator(Operator::Increment)),
            mt("foo.c", 3, 4, MacroTokenType::Operator(Operator::Decrement)),
            mt("foo.c", 6, 7, MacroTokenType::Operator(Operator::Increment)),
            mt("foo.c", 8, 9, MacroTokenType::Operator(Operator::Increment)),
            mt("foo.c", 10, 10, MacroTokenType::Operator(Operator::Plus)),
        ],
    );
}

#[test]
fn period() {
    process(
        ". ... .a.",
        vec![
            mt("foo.c", 0, 0, MacroTokenType::Operator(Operator::Dot)),
            mt(
                "foo.c",
                2,
                4,
                MacroTokenType::Punctuation(Punctuation::Varargs),
            ),
            mt("foo.c", 6, 6, MacroTokenType::Operator(Operator::Dot)),
            mt("foo.c", 7, 7, MacroTokenType::Identifier("a".to_string())),
            mt("foo.c", 8, 8, MacroTokenType::Operator(Operator::Dot)),
        ],
    );
}

#[test]
fn comments() {
    process(
        "/* foo */ bar //baz \nqux // bax \\\nbux \n/**/ asd",
        vec![
            mt(
                "foo.c",
                10,
                12,
                MacroTokenType::Identifier("bar".to_string()),
            ),
            mt(
                "foo.c",
                21,
                23,
                MacroTokenType::Identifier("qux".to_string()),
            ),
            mt(
                "foo.c",
                44,
                46,
                MacroTokenType::Identifier("asd".to_string()),
            ),
        ],
    );
}

#[test]
fn whitespace() {
    process("\t   \t \\\n \n   ", vec![]);
}

#[test]
fn spurious_backslash() {
    process(
        "\\a",
        vec![
            mt("foo.c", 0, 0, MacroTokenType::Other('\\')),
            mt("foo.c", 1, 1, MacroTokenType::Identifier("a".to_string())),
        ],
    );
}

#[test]
fn simple_macro() {
    process(
        "#define FOO BAR\nFOO",
        vec![mt_s(
            "foo.c",
            16,
            18,
            MacroTokenType::Identifier("BAR".to_string()),
            Some(s("foo.c", 0, 15, Some(s("foo.c", 12, 14, None)))),
        )],
    );
}

#[test]
fn simple_macro_with_offset() {
    process(
        "\n\n#define FOO BAR\nFOO",
        vec![mt_s(
            "foo.c",
            18,
            20,
            MacroTokenType::Identifier("BAR".to_string()),
            Some(s("foo.c", 2, 17, Some(s("foo.c", 14, 16, None)))),
        )],
    );
}

#[test]
fn multiple_macros() {
    process(
        "#define BAR BAZ\n#define FOO BAR\nFOO",
        vec![mt_s(
            "foo.c",
            32,
            34,
            MacroTokenType::Identifier("BAZ".to_string()),
            Some(s(
                "foo.c",
                16,
                31, // #define BAR BAZ
                Some(s(
                    "foo.c",
                    28,
                    30, // BAZ
                    Some(s(
                        "foo.c",
                        0,
                        15, // #define FOO BAR
                        Some(s(
                            "foo.c", 12, 14, // BAR
                            None,
                        )),
                    )),
                )),
            )),
        )],
    );
}

#[test]
fn recursive_macro() {
    process(
        "\n\n#define FOO FOO\nFOO",
        vec![mt_s(
            "foo.c",
            18,
            20,
            MacroTokenType::Identifier("FOO".to_string()),
            Some(s("foo.c", 2, 17, Some(s("foo.c", 14, 16, None)))),
        )],
    );
}

#[test]
fn mutually_recursive_macros() {
    process(
        "#define BAR FOO\n#define FOO BAR\nFOO",
        vec![mt_s(
            "foo.c",
            32,
            34,
            MacroTokenType::Identifier("FOO".to_string()),
            Some(s(
                "foo.c",
                16,
                31, // #define BAR BAZ
                Some(s(
                    "foo.c",
                    28,
                    30, // BAZ
                    Some(s(
                        "foo.c",
                        0,
                        15, // #define FOO BAR
                        Some(s(
                            "foo.c", 12, 14, // BAR
                            None,
                        )),
                    )),
                )),
            )),
        )],
    );
}

#[test]
fn included_macro() {
    process_files(
        vec![
            ("foo.h", "#define FOO foo\n"),
            ("bar.h", "#include \"foo.h\"\nFOO"),
        ],
        "bar.h",
        vec![mt_s(
            "bar.h",
            17,
            19, // FOO
            MacroTokenType::Identifier("foo".to_string()),
            Some(s(
                "foo.h",
                0,
                15, // #define FOO foo
                Some(s(
                    "foo.h", 12, 14, // foo
                    None,
                )),
            )),
        )],
    )
}

#[test]
fn many_includes() {
    process_files(
        vec![
            ("a.h", " "),
            ("b.h", " "),
            ("main.h", "#include \"a.h\"\n#include \"b.h\"\n//"),
        ],
        "main.h",
        vec![],
    )
}

#[test]
fn undef() {
    process("#undef FOO\n#define FOO\n#undef FOO", vec![]);
}

#[test]
fn sizeof_types() {
    process(
        "sizeof(int)",
        vec![ mt("foo.c", 0, 10, MacroTokenType::Sizeof(SizeofExpression::Static(8)))]);
    process(
        "sizeof(int *)",
        vec![ mt("foo.c", 0, 12, MacroTokenType::Sizeof(SizeofExpression::Static(8)))]);
    process(
        "sizeof(*int)",
        vec![ mt("foo.c", 0, 11, MacroTokenType::Sizeof(SizeofExpression::Static(8)))]);
    process(
        "sizeof((1))",
        vec![ mt("foo.c", 0, 10, MacroTokenType::Sizeof(SizeofExpression::Static(8)))]);
    process(
        "sizeof(((1)))",
        vec![ mt("foo.c", 0, 12, MacroTokenType::Sizeof(SizeofExpression::Static(8)))]);
    process(
        "sizeof ( 1 ) ",
        vec![ mt("foo.c", 0, 11, MacroTokenType::Sizeof(SizeofExpression::Static(8)))]);
    process(
        "sizeof(1+1)",
        vec![ mt("foo.c", 0, 10, MacroTokenType::Sizeof(SizeofExpression::Static(8)))]);
    process(
        "sizeof int",
        vec![ mt("foo.c", 0, 9, MacroTokenType::Sizeof(SizeofExpression::Static(8)))]);
}

