#[macro_use]
pub mod macrotoken;
pub mod calculator;
pub mod tokenizer;
pub mod tokentype;

use macrotoken::MacroToken;
use resolve::{FileQuery, ResolveResult};
use tokenizer::{MacroContext, ParseResult};

pub struct PreprocessorOptions<'a> {
    pub include_path: Vec<&'a str>,
    pub include_files: Vec<&'a str>,
    pub definitions: Vec<(&'a str, &'a str)>,
}

pub fn preprocess<CB>(
    get_file: CB,
    filename: &str,
    ctx: &mut Option<MacroContext>,
) -> ParseResult<Vec<MacroToken>>
where
    CB: Fn(FileQuery) -> ResolveResult,
{
    let context = ctx.get_or_insert_with(MacroContext::new);
    Ok(context.preprocess_file(filename, get_file))
}

pub fn preprocess_file<CB>(
    filename: &str,
    get_file: CB,
    options: &PreprocessorOptions,
    ctx: &mut Option<MacroContext>,
) -> ParseResult<Vec<MacroToken>>
where
    CB: Fn(FileQuery) -> ResolveResult,
{
    let context = ctx.get_or_insert_with(|| {
        let mut context = MacroContext::new();
        context.add_definitions(&options.definitions, &get_file);
        for filename in &options.include_files {
            context.preprocess_file(filename, &get_file);
        }
        context
    });
    Ok(context.preprocess_file(filename, &get_file))
}

pub fn preprocess_str<CB>(
    content: &str,
    get_file: CB,
    options: &PreprocessorOptions,
    ctx: &mut Option<MacroContext>,
) -> ParseResult<Vec<MacroToken>>
where
    CB: Fn(FileQuery) -> ResolveResult,
{
    dbg!(&content);
    let context = ctx.get_or_insert_with(|| {
        let mut context = MacroContext::new();
        context.add_definitions(&options.definitions, &get_file);
        for filename in &options.include_files {
            context.preprocess_file(filename, &get_file);
        }
        context
    });
    Ok(context.preprocess_str(content, &get_file))
}
