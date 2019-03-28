#[macro_use]
pub mod macrotoken;
pub mod tokenizer;
pub mod tokentype;
pub mod calculator;

use macrotoken::MacroToken;
use tokenizer::{MacroContext, ParseResult};
use fragment::fragment::FragmentIterator;

pub struct PreprocessorOptions<'a> {
    pub include_path: Vec<&'a str>,
    pub include_files: Vec<&'a str>,
    pub definitions: Vec<(&'a str, &'a str)>,
}

pub fn preprocess<CB>(get_file: CB, filename: &str)
    -> ParseResult<(Vec<MacroToken>, FragmentIterator)>
where
    CB: Fn(bool, String, String) -> (String, String)
{
    let mut context = MacroContext::new(get_file);
    Ok(context.preprocess(filename))
}

pub fn preprocess_file<CB>(filename: &str, get_file: CB, options: &PreprocessorOptions)
    -> ParseResult<(Vec<MacroToken>, FragmentIterator)>
where
    CB: Fn(bool, String, String) -> (String, String)
{
    let mut context = MacroContext::new(get_file);
    context.add_definitions(&options.definitions);
    for filename in &options.include_files {
        context.preprocess(filename);
    }
    Ok(context.preprocess(filename))
}
