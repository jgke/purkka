extern crate preprocessor;
extern crate shared;
extern crate ctoken;

use shared::fragment::{FragmentIterator, Source, Span};

use preprocessor::macrotoken::{MacroToken, MacroTokenType};
use preprocessor::tokenizer::{ParseResult};
use preprocessor::tokentype::{Operator, Punctuation};
use ctoken::token::SizeofExpression;

fn preprocess_string(filename: &str, content: &str) -> ParseResult<Vec<MacroToken>> {
    preprocessor::preprocess(
        |_is_quoted, _current_file, f| {
            assert_eq!(filename, f);
            (content.to_string(), f)
        },
        filename,
    ).map(|t| t.0)
}

fn process_files(files: Vec<(&str, &str)>, start: &str, expected: Vec<MacroToken>) {
    let mut iter = FragmentIterator::new("_empty", " ");

    println!("Processing file contents:");
    for (name, content) in &files {
        println!("---- File {} ----\n{}", name, content);
        iter.split_and_push_file(name, content);
    }
    println!("---- End file list ----");

    let processed = preprocessor::preprocess(
        |_is_quoted, _current_file, filename| {
            for (name, content) in &files {
                if name == &filename {
                    return (content.to_string(), filename);
                }
            }
            panic!()
        },
        start,
    );

    if let Ok(p) = &processed {
        println!("---- Test result ----");
        println!("Result:");
        p.0.iter().for_each(|t| println!("{}", t.display(&iter)));
        println!("Expected:");
        expected
            .iter()
            .for_each(|t| println!("{}", t.display(&iter)));
    }
    assert_eq!(processed.map(|t| t.0), Ok(expected));
}

fn process(original: &str, expected: Vec<MacroToken>) {
    println!(
        "Processing file contents:\n---- Start file ----\n{}\n---- End file ----",
        original
    );
    let iter = FragmentIterator::new("foo.c", original);
    let processed = preprocess_string("foo.c", original);
    if let Ok(p) = &processed {
        println!("---- Test result ----");
        println!("Result:");
        p.iter().for_each(|t| println!("{}", t.display(&iter)));
        println!("Expected:");
        expected
            .iter()
            .for_each(|t| println!("{}", t.display(&iter)));
    }
    assert_eq!(processed, Ok(expected));
}

fn mt(file: &str, lo: usize, hi: usize, ty: MacroTokenType) -> MacroToken {
    MacroToken {
        source: s(file, lo, hi, None),
        ty,
    }
}

fn mt_s(
    file: &str,
    lo: usize,
    hi: usize,
    ty: MacroTokenType,
    source: Option<Source>,
) -> MacroToken {
    MacroToken {
        source: s(file, lo, hi, source),
        ty,
    }
}

fn s(file: &str, lo: usize, hi: usize, s: Option<Source>) -> Source {
    Source {
        filename: file.to_string(),
        span: Span {
            lo,
            hi,
            source: s.map(|s| Box::new(s)),
        },
    }
}

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
fn function_macro_constant() {
    process(
        "#define FOO() foo\nFOO()",
        vec![mt_s(
            "foo.c",
            14,
            16, // foo
            MacroTokenType::Identifier("foo".to_string()),
            Some(s("foo.c", 0, 17, None)),
        )],
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
            MacroTokenType::Identifier("b".to_string()),
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
            MacroTokenType::Identifier("b".to_string()),
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
            MacroTokenType::Identifier("FOO".to_string()),
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
                MacroTokenType::Number("1".to_string()),
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
                MacroTokenType::Number("2".to_string()),
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
fn token_pasting() {
    process(
        "#define FOO a##b\nFOO",
        vec![mt_s(
            "foo.c",
            17,
            19, // FOO
            MacroTokenType::Identifier("ab".to_string()),
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
            MacroTokenType::Identifier("BARb".to_string()),
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
            MacroTokenType::Identifier("foobar".to_string()),
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
            MacroTokenType::Identifier("foobarbaz".to_string()),
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
fn function_macro_with_trailing_paren() {
    process(
        "#define FOO(a) a)\nFOO(foo))",
        vec![
            mt_s(
                "foo.c",
                22,
                24, // foo
                MacroTokenType::Identifier("foo".to_string()),
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
fn stringify_function_macro() {
    process(
        "#define FOO(a) #a\nFOO(foo)",
        vec![mt_s(
            "foo.c",
            22,
            24, // foo
            MacroTokenType::StringLiteral("foo".to_string()),
            Some(s(
                "foo.c",
                0,
                17, // #define FOO(a) #a
                None),
            )),
        ],
    );
}

#[test]
fn stringify_function_macro_no_expand() {
    process(
        "#define BAR(a,b) a b\n#define FOO(a, b) #a b\nFOO(BAR(), BAR(1,2))",
        vec![mt_s(
            "foo.c",
            48,
            52, // foo##a
            MacroTokenType::StringLiteral("BAR()".to_string()),
            Some(s(
                "foo.c",
                21,
                43, // #define FOO(a, b) #a b
                None),
            )),
            mt_s(
            "foo.c",
            59,
            59, // 1
            MacroTokenType::Number("1".to_string()),
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
                                    "foo.c",
                                    21,
                                    43, // #define FOO(a, b) #a b
                                    None))),
            )))))))))),
            mt_s(
            "foo.c",
            61,
            61, // 2
            MacroTokenType::Number("2".to_string()),
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
                                    "foo.c",
                                    21,
                                    43, // #define FOO(a, b) #a b
                                    None))),
            )))))))))),
        ],
    );
}

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
fn undef() {
    process("#undef FOO\n#define FOO\n#undef FOO", vec![]);
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
}

#[test]
fn variadic_macros() {
    //process(
    //    "#define FOO(...) __VA_ARGS__\nFOO()\nFOO(a)\nFOO(a,b)\nFOO(a,b,c)\n",
    //    vec![ mt("foo.c", 0, 10, MacroTokenType::Sizeof(SizeofExpression::Static(8)))]);
    process(
        "#define FOO(...) __VA_ARGS__\nFOO()",
        vec![],
    );
    process(
        "#define FOO(...) __VA_ARGS__\nFOO(a)",
        vec![mt_s(
            "foo.c",
            33,
            33, // a
            MacroTokenType::Identifier("a".to_string()),
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
        vec![mt_s(
            "foo.c",
            33,
            33, // a
            MacroTokenType::Identifier("a".to_string()),
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
            MacroTokenType::Identifier("b".to_string()),
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
        )
        ],
    );
}

#[test]
fn variadic_named_macros() {
    //process(
    //    "#define FOO(...) __VA_ARGS__\nFOO()\nFOO(a)\nFOO(a,b)\nFOO(a,b,c)\n",
    //    vec![ mt("foo.c", 0, 10, MacroTokenType::Sizeof(SizeofExpression::Static(8)))]);
    process(
        "#define FOO(a...) a\nFOO(a)",
        vec![mt_s(
            "foo.c",
            24,
            24, // a
            MacroTokenType::Identifier("a".to_string()),
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
            MacroTokenType::Identifier("a".to_string()),
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
            MacroTokenType::Identifier("b".to_string()),
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

// todo: test for eof after "#define foo" and "#define"
