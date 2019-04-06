#![feature(test)]

extern crate test;

use test::Bencher;

use fragment::fragment::{FragmentIterator, Source, Span};
use debug::debug::DEBUG_VALS;

use preprocessor::macrotoken::{MacroToken, MacroTokenType};
use preprocessor::tokenizer::{ParseResult};

pub fn process_files(files: Vec<(&str, &str)>, start: &str) -> Result<(Vec<MacroToken>, FragmentIterator), &'static str> {
    let processed = preprocessor::preprocess(
        |_is_quoted, _current_file, filename| {
            for (name, content) in &files {
                if name == &filename {
                    return (content.to_string(), filename);
                }
            }
            panic!("Tried to open file {}", filename)
        },
        start,
    );

    assert!(processed.is_ok());
    processed
}

#[bench]
fn expansion_bench(b: &mut Bencher) {
    let file = "
#define FOO(a, b, c, d, e, f, g, h) a b c##d e f g h
#define BAR(a, b, c, d, e, f, g, h) \
        FOO(a, b, c, d, e, f, g, h) \
        FOO(a, b, c, d, e, f, g, h) \
        FOO(a, b, c, d, e, f, g, h) \
        FOO(a, b, c, d, e, f, g, h) \
        FOO(a, b, c, d, e, f, g, h) \
        FOO(a, b, c, d, e, f, g, h) \
        FOO(a, b, c, d, e, f, g, h) \
        FOO(a, b, c, d, e, f, g, h)
#define BAZ(a, b, c, d, e, f, g, h) \
        BAR(a, b, c, d, e, f, g, h) \
        BAR(a, b, c, d, e, f, g, h) \
        BAR(a, b, c, d, e, f, g, h) \
        BAR(a, b, c, d, e, f, g, h) \
        BAR(a, b, c, d, e, f, g, h) \
        BAR(a, b, c, d, e, f, g, h) \
        BAR(a, b, c, d, e, f, g, h) \
        BAR(a, b, c, d, e, f, g, h)
BAZ(\"Complex\", stream, of, various, 123elements, which_should, /* work */ just_as_expected, ++)
        ";
    b.iter(|| process_files(vec![("main.c", file)], "main.c"))
}
