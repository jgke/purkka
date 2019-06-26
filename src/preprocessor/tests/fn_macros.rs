extern crate ctoken;
extern crate preprocessor;
extern crate shared;

mod common;

use common::*;
use preprocessor::macrotoken::MacroTokenType;
use preprocessor::tokentype::Punctuation;

#[test]
fn function_macro_constant() {
    process(
        "#define FOO() foo\nFOO()",
        vec![mt_s(
            "foo.c",
            14,
            16, // foo
            ident("foo"),
            Some(s("foo.c", 0, 17, None)),
        )],
    );
}

#[test]
fn function_macro_two() {
    process(
        "#define BAR(a) a\n#define FOO() BAR(foo) BAR(foo)\nFOO()",
        vec![mt_s(
            "foo.c",
            14,
            16, // foo
            ident("foo"),
            Some(s("foo.c", 0, 17, None)),
        ), mt_s(
            "foo.c",
            16,
            18, // this is wrong
            ident("foo"),
            Some(s("foo.c", 0, 17, None)),
        )
        ],
    );
}

#[test]
fn function_macro_three() {
    process(
        "#define BAZ(a) a\n#define BAR(a) BAZ(a) BAZ(a)\n#define FOO() BAR(foo) BAR(foo)\nFOO()",
        vec![mt_s(
            "foo.c",
            14,
            16, // foo
            ident("foo"),
            Some(s("foo.c", 0, 17, None)),
        ), mt_s(
            "foo.c",
            16,
            18, // this is wrong
            ident("foo"),
            Some(s("foo.c", 0, 17, None)),
        ), mt_s(
            "foo.c",
            16,
            18, // this is wrong
            ident("foo"),
            Some(s("foo.c", 0, 17, None)),
        ), mt_s(
            "foo.c",
            16,
            18, // this is wrong
            ident("foo"),
            Some(s("foo.c", 0, 17, None)),
        )
        ],
    );
}

#[test]
fn function_macro_one_arg() {
    process(
        "#define FOO(a) a\nFOO(b)",
        vec![mt_s(
            "foo.c",
            21,
            21, // b
            ident("b"),
            Some(s(
                "foo.c",
                17,
                22, // FOO(b)
                Some(s(
                    "foo.c",
                    15,
                    15, // a
                    Some(s(
                        "foo.c", 0, 16, // #define FOO(a) a
                        None,
                    )),
                )),
            )),
        )],
    );
}

#[test]
fn function_macro_nested() {
    process(
        "#define BAR(a) a\n#define FOO(a) BAR(a)\nFOO(b)",
        vec![mt_s(
            "foo.c",
            43,
            43, // b
            ident("b"),
            Some(s(
                "foo.c",
                39,
                44, // FOO(b)
                Some(s(
                    "foo.c",
                    36,
                    36, // a, second inside FOO(a) BAR(a)
                    Some(s(
                        "foo.c",
                        17,
                        38, // #define FOO(a) BAR(a)
                        Some(s(
                            "foo.c",
                            15,
                            15, // a, second inside BAR(a) a
                            Some(s(
                                "foo.c", 0, 16, // #define BAR(a) a
                                None,
                            )),
                        )),
                    )),
                )),
            )),
        )],
    );
}

#[test]
fn macro_expand_infinite_recursive() {
    process(
        "#define FOO(a) FOO\nFOO(a)",
        vec![mt_s(
            "foo.c",
            15,
            17, // FOO
            ident_t("FOO"),
            Some(s(
                "foo.c", 0, 18, // #define FOO(a) FOO
                None,
            )),
        )],
    );
}

#[test]
fn function_macro_multiple_arguments() {
    process(
        "#define FOO(a,b) a b\nFOO(1,2)",
        vec![
            mt_s(
                "foo.c",
                25,
                25, // 1
                MacroTokenType::Number(From::from("1")),
                Some(s(
                    "foo.c",
                    21,
                    28, // FOO(1,2)
                    Some(s(
                        "foo.c",
                        17,
                        17, // a
                        Some(s(
                            "foo.c", 0, 20, // #define FOO(a) FOO(a,b) a b
                            None,
                        )),
                    )),
                )),
            ),
            mt_s(
                "foo.c",
                27,
                27, // 2,
                MacroTokenType::Number(From::from("2")),
                Some(s(
                    "foo.c",
                    21,
                    28, // FOO(1,2)
                    Some(s(
                        "foo.c",
                        19,
                        19, // a
                        Some(s(
                            "foo.c", 0, 20, // #define FOO(a) FOO(a,b) a b
                            None,
                        )),
                    )),
                )),
            ),
        ],
    );
}

#[test]
fn function_macro_with_trailing_paren() {
    process(
        "#define FOO(a) a)\nFOO(foo))",
        vec![
            mt_s(
                "foo.c",
                22,
                24, // foo
                ident("foo"),
                Some(s(
                    "foo.c",
                    18,
                    25, // FOO(foo)
                    Some(s(
                        "foo.c",
                        15,
                        15, // a
                        Some(s(
                            "foo.c", 0, 17, // #define FOO(a) a)
                            None,
                        )),
                    )),
                )),
            ),
            mt_s(
                "foo.c",
                16,
                16, // )
                MacroTokenType::Punctuation(Punctuation::CloseParen),
                Some(s(
                    "foo.c", 0, 17, // #define FOO(a) a)
                    None,
                )),
            ),
            mt_s(
                "foo.c",
                26,
                26, // )
                MacroTokenType::Punctuation(Punctuation::CloseParen),
                None,
            ),
        ],
    );
}

#[test]
fn variadic_macros() {
    //process(
    //    "#define FOO(...) __VA_ARGS__\nFOO()\nFOO(a)\nFOO(a,b)\nFOO(a,b,c)\n",
    //    vec![ mt("foo.c", 0, 10, MacroTokenType::Sizeof(SizeofExpression::Static(8)))]);
    process("#define FOO(...) __VA_ARGS__\nFOO()", vec![]);
    process("#define FOO(a, ...) __VA_ARGS__\nFOO(a)", vec![]);
    process(
        "#define FOO(...) __VA_ARGS__\nFOO(a)",
        vec![mt_s(
            "foo.c",
            33,
            33, // a
            ident("a"),
            Some(s(
                "foo.c",
                29,
                34, // FOO(a)
                Some(s(
                    "foo.c",
                    17,
                    27, // __VA_ARGS__
                    Some(s(
                        "foo.c", 0, 28, // #define FOO(a) __VA_ARGS__
                        None,
                    )),
                )),
            )),
        )],
    );
    process(
        "#define FOO(...) __VA_ARGS__\nFOO(a, b)",
        vec![
            mt_s(
                "foo.c",
                33,
                33, // a
                ident("a"),
                Some(s(
                    "foo.c",
                    29,
                    37, // FOO(a, b)
                    Some(s(
                        "foo.c",
                        17,
                        27, // __VA_ARGS__
                        Some(s(
                            "foo.c", 0, 28, // #define FOO(a) __VA_ARGS__
                            None,
                        )),
                    )),
                )),
            ),
            mt_s(
                "foo.c",
                34,
                34, // ,
                MacroTokenType::Punctuation(Punctuation::Comma),
                Some(s(
                    "foo.c",
                    29,
                    37, // FOO(a, b)
                    Some(s(
                        "foo.c",
                        17,
                        27, // __VA_ARGS__
                        Some(s(
                            "foo.c", 0, 28, // #define FOO(a) __VA_ARGS__
                            None,
                        )),
                    )),
                )),
            ),
            mt_s(
                "foo.c",
                36,
                36, // b
                ident("b"),
                Some(s(
                    "foo.c",
                    29,
                    37, // FOO(a, b)
                    Some(s(
                        "foo.c",
                        17,
                        27, // __VA_ARGS__
                        Some(s(
                            "foo.c", 0, 28, // #define FOO(a) __VA_ARGS__
                            None,
                        )),
                    )),
                )),
            ),
        ],
    );
}

#[test]
fn variadic_named_macros() {
    process("#define FOO(a...) a \nFOO()", vec![]);
    process(
        "#define FOO(a, b...) a b\nFOO(1)",
        vec![mt_s(
            "foo.c",
            29,
            29, // a
            MacroTokenType::Number(From::from("1")),
            Some(s(
                "foo.c",
                25,
                30, // FOO(a)
                Some(s(
                    "foo.c",
                    21,
                    21, // a
                    Some(s(
                        "foo.c", 0, 24, // #define FOO(...a) a
                        None,
                    )),
                )),
            )),
        )],
    );
    process(
        "#define FOO(a...) a\nFOO(a)",
        vec![mt_s(
            "foo.c",
            24,
            24, // a
            ident("a"),
            Some(s(
                "foo.c",
                20,
                25, // FOO(a)
                Some(s(
                    "foo.c",
                    18,
                    18, // a
                    Some(s(
                        "foo.c", 0, 19, // #define FOO(...a) a
                        None,
                    )),
                )),
            )),
        )],
    );
    process(
        "#define FOO(a...) a\nFOO(a, b)",
        vec![
            mt_s(
                "foo.c",
                24,
                24, // a
                ident("a"),
                Some(s(
                    "foo.c",
                    20,
                    28, // FOO(a)
                    Some(s(
                        "foo.c",
                        18,
                        18, // a
                        Some(s(
                            "foo.c", 0, 19, // #define FOO(...a) a
                            None,
                        )),
                    )),
                )),
            ),
            mt_s(
                "foo.c",
                25,
                25, // ,
                MacroTokenType::Punctuation(Punctuation::Comma),
                Some(s(
                    "foo.c",
                    20,
                    28, // FOO(a)
                    Some(s(
                        "foo.c",
                        18,
                        18, // a
                        Some(s(
                            "foo.c", 0, 19, // #define FOO(...a) a
                            None,
                        )),
                    )),
                )),
            ),
            mt_s(
                "foo.c",
                27,
                27, // a
                ident("b"),
                Some(s(
                    "foo.c",
                    20,
                    28, // FOO(a)
                    Some(s(
                        "foo.c",
                        18,
                        18, // a
                        Some(s(
                            "foo.c", 0, 19, // #define FOO(...a) a
                            None,
                        )),
                    )),
                )),
            ),
        ],
    );
}

#[test]
fn invalid_macros() {
    macro_panics("#define FOO(a, b) a\nFOO(a)");
    macro_panics("#define FOO(a, b, c) a\nFOO(a, b)");
    macro_panics("#define FOO() a\nFOO(a)");
    macro_panics("#define FOO(a) a\nFOO(a, b)");
    macro_panics("#define FOO(a, b) a\nFOO(a, b, c)");
    macro_panics("#define FOO(a, b, c) a\nFOO(a, b, c, d)");
}

#[test]
fn weird_stuff() {
    process("#define ARGS(...) __VA_ARGS__\n#define BAR(a) \n#define FOO(...) BAR(ARGS(__VA_ARGS__))\nFOO(1, 2)", vec![]);
    process("#define __and(x, y)	___and(x, y)", vec![]);
    //                          ^ \t
    process(
        "
#define FOO(a, b) b
FOO(,c)
            ",
        vec![mt_s(
            "foo.c",
            26,
            26, // foo
            ident("c"),
            Some(s(
                "foo.c",
                21,
                27,
                Some(s("foo.c", 19, 19, Some(s("foo.c", 1, 20, None)))),
            )),
        )],
    );
    process(
        "
#define _ARG_COUNT(_0, _1, _n, ...) _n
#define ARG_COUNT(...) _ARG_COUNT(, ##__VA_ARGS__, 1, 0)

ARG_COUNT(a)
            ",
        vec![mt_s(
            "foo.c",
            91,
            91, // foo
            MacroTokenType::Number(From::from("1")),
            Some(s(
                "foo.c",
                40,
                96,
                Some(s("foo.c", 37, 38, Some(s("foo.c", 1, 39, None)))),
            )),
        )],
    );
    process(
        "#define BAR(a)\n#define FOO(a) BAR(sizeof(a))\nFOO(int)",
        vec![],
    );
    process("#define BAR(a)\nBAR()", vec![]);
    process("#define BAR(a,b)\nBAR(a,)", vec![]);
    process("#define BAR(...) ,##__VA_ARGS__\nBAR()", vec![]);
}
