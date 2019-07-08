#![feature(test)]

extern crate test;

use test::Bencher;

use fragment::fragment::FragmentIterator;
use resolve::*;

use preprocessor::macrotoken::MacroToken;

pub fn process_files(
    files: Vec<(&str, &str)>,
    start: &str,
) -> Result<(Vec<MacroToken>, FragmentIterator), &'static str> {
    let processed = preprocessor::preprocess(
        |req| {
            assert_eq!(req.need_raw, true);
            for (name, content) in &files {
                if name == &req.requested_file {
                    return ResolveResult::new_raw(name, content);
                }
            }
            panic!("Tried to open file {}", req.requested_file)
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

#[bench]
fn exponential_bench(b: &mut Bencher) {
    let file = "
#define FOO1(a, b, c, d, e, f, g, h) FOO1(a, b, c, d, e, f, g, h)
#define FOO2(a, b, c, d, e, f, g, h) \
        FOO1(a, b, c, d, e, f, g, h) \
        FOO1(a, b, c, d, e, f, g, h) \
#define FOO3(a, b, c, d, e, f, g, h) \
        FOO2(a, b, c, d, e, f, g, h) \
        FOO2(a, b, c, d, e, f, g, h) \
#define FOO4(a, b, c, d, e, f, g, h) \
        FOO3(a, b, c, d, e, f, g, h) \
        FOO3(a, b, c, d, e, f, g, h) \
#define FOO5(a, b, c, d, e, f, g, h) \
        FOO4(a, b, c, d, e, f, g, h) \
        FOO4(a, b, c, d, e, f, g, h) \
#define FOO6(a, b, c, d, e, f, g, h) \
        FOO5(a, b, c, d, e, f, g, h) \
        FOO5(a, b, c, d, e, f, g, h) \
FOO6(lots, of, different, strings, to, expand, for, debugs)
        ";
    b.iter(|| process_files(vec![("main.c", file)], "main.c"))
}
