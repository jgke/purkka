#![allow(dead_code)]

use debug::debug::DEBUG_VALS;
use fragment::fragment::{FragmentIterator, Source, Span};
use resolve::*;

use preprocessor::macrotoken::{MacroToken, MacroTokenType};
use preprocessor::tokenizer::ParseResult;

pub fn preprocess_string(filename: &str, content: &str) -> ParseResult<Vec<MacroToken>> {
    for s in DEBUG_VALS {
        if std::env::var(s).is_err() {
            std::env::set_var(s, "1");
        }
    }
    preprocessor::preprocess(
        |req| {
            assert_eq!(filename, req.requested_file);
            assert_eq!(req.need_raw, true);
            ResolveResult::new_raw(filename, content)
        },
        filename,
    )
    .map(|t| t.0)
}

pub fn process_files(files: Vec<(&str, &str)>, start: &str, expected: Vec<MacroToken>) {
    let mut iter = FragmentIterator::new("_empty", " ");

    println!("Processing file contents:");
    for (name, content) in &files {
        println!("---- File {} ----\n{}", name, content);
        iter.split_and_push_file(name, &content);
    }
    println!("---- End file list ----");

    let processed = preprocessor::preprocess(
        |req| {
            assert_eq!(req.need_raw, true);
            for (name, content) in &files {
                if name == &req.requested_file {
                    return ResolveResult::new_raw(name, content);
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

pub fn process(original: &str, expected: Vec<MacroToken>) {
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

pub fn mt(file: &str, lo: usize, hi: usize, ty: MacroTokenType) -> MacroToken {
    MacroToken {
        source: s(file, lo, hi, None),
        ty,
    }
}

pub fn mt_s(
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

pub fn s(file: &str, lo: usize, hi: usize, s: Option<Source>) -> Source {
    Source {
        filename: From::from(file),
        span: Span {
            lo,
            hi,
            source: s.map(|s| Box::new(s)),
        },
    }
}

pub fn macro_panics(arg: &str) {
    let result = std::panic::catch_unwind(|| {
        println!("The following file should panic:\n{}", arg);
        assert!(preprocess_string("foo.c", arg).is_err());
    });
    assert!(result.is_err());
}

pub fn ident(arg: &str) -> MacroTokenType {
    MacroTokenType::Identifier(From::from(arg))
}
