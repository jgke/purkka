extern crate ctoken;
extern crate preprocessor;
extern crate shared;

mod common;

use common::*;
use preprocessor::macrotoken::MacroTokenType;

#[test]
fn token_pasting() {
    process(
        "#define FOO a##b\nFOO",
        vec![mt_s(
            "foo.c",
            17,
            19, // FOO
            ident("ab"),
            Some(s(
                "foo.c",
                0,
                16, // #define FOO a##b
                Some(s("foo.c", 12, 15, None)),
            )),
        )],
    );
}

#[test]
fn token_pasting_with_macro_content() {
    process(
        "#define BAR foo\n#define FOO BAR##b\nFOO",
        vec![mt_s(
            "foo.c",
            35,
            37, // FOO
            ident("BARb"),
            Some(s(
                "foo.c",
                16,
                34, // #define FOO BAR##b
                Some(s(
                    "foo.c", 28, 33, // BAR##b
                    None,
                )),
            )),
        )],
    );
}

#[test]
fn token_pasting_with_function_macro() {
    process(
        "#define FOO(a,b) a##b\nFOO(foo,bar)",
        vec![mt_s(
            "foo.c",
            17,
            20, // a##b
            ident("foobar"),
            Some(s(
                "foo.c",
                22,
                33, // FOO(foo,bar)
                Some(s(
                    "foo.c", 0, 21, // #define FOO(a,b) a##b
                    None,
                )),
            )),
        )],
    );
}

#[test]
fn function_macro_with_expanded_argument() {
    process(
        "#define FOO(a) foo##a\n#define BAR(a,b) a##b\nFOO(BAR(bar,baz))",
        vec![mt_s(
            "foo.c",
            15,
            20, // foo##a
            ident("foobarbaz"),
            Some(s(
                "foo.c",
                44,
                60, // FOO(BAR(bar,baz))
                Some(s(
                    "foo.c", 0, 21, // #define FOO(a) foo##a
                    None,
                )),
            )),
        )],
    );
}

#[test]
fn stringify_function_macro() {
    process(
        "#define FOO(a) #a\nFOO(foo)",
        vec![mt_s(
            "foo.c",
            22,
            24, // foo
            MacroTokenType::StringLiteral(From::from("foo")),
            Some(s(
                "foo.c", 0, 17, // #define FOO(a) #a
                None,
            )),
        )],
    );
}

#[test]
fn stringify_function_macro_no_expand() {
    process(
        "#define BAR(a,b) a b\n#define FOO(a, b) #a b\nFOO(BAR(), BAR(1,2))",
        vec![
            mt_s(
                "foo.c",
                48,
                52, // foo##a
                MacroTokenType::StringLiteral(From::from("BAR()")),
                Some(s(
                    "foo.c", 21, 43, // #define FOO(a, b) #a b
                    None,
                )),
            ),
            mt_s(
                "foo.c",
                59,
                59, // 1
                MacroTokenType::Number(From::from("1")),
                Some(s(
                    "foo.c",
                    55,
                    62, // BAR(1,2)
                    Some(s(
                        "foo.c",
                        17,
                        17, // a
                        Some(s(
                            "foo.c",
                            0,
                            20, // #define BAR(a,b) a b
                            Some(s(
                                "foo.c",
                                44,
                                63, // FOO(BAR(), BAR(1,2))
                                Some(s(
                                    "foo.c",
                                    42,
                                    42, // b
                                    Some(s(
                                        "foo.c", 21, 43, // #define FOO(a, b) #a b
                                        None,
                                    )),
                                )),
                            )),
                        )),
                    )),
                )),
            ),
            mt_s(
                "foo.c",
                61,
                61, // 2
                MacroTokenType::Number(From::from("2")),
                Some(s(
                    "foo.c",
                    55,
                    62, // BAR(1,2)
                    Some(s(
                        "foo.c",
                        19,
                        19, // b
                        Some(s(
                            "foo.c",
                            0,
                            20, // #define BAR(a,b) a b
                            Some(s(
                                "foo.c",
                                44,
                                63, // FOO(BAR(), BAR(1,2))
                                Some(s(
                                    "foo.c",
                                    42,
                                    42, // b
                                    Some(s(
                                        "foo.c", 21, 43, // #define FOO(a, b) #a b
                                        None,
                                    )),
                                )),
                            )),
                        )),
                    )),
                )),
            ),
        ],
    );
}
