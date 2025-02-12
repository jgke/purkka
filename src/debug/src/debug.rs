pub enum DebugVal {
    IncludeName,
    DumpLalrTable,
    LalrRuntime,
    DebugFragment,
    MacroExpand,
    CParserToken,
    Core,
}

pub static DEBUG_VALS: &[&str] = &[
    "DEBUG_INCLUDE_NAME",
    "DEBUG_DUMP_LALR_TABLE",
    "DEBUG_LALR_RUNTIME",
    "DEBUG_FRAGMENT",
    "DEBUG_MACRO_EXPAND",
    "DEBUG_CPARSER_TOKEN",
    "DEBUG_CORE",
];

#[cfg(debug_assertions)]
pub fn is_debug_enabled(ident: DebugVal) -> bool {
    if let Ok("1") = std::env::var(DEBUG_VALS[ident as usize])
        .as_ref()
        .map(String::as_str)
    {
        true
    } else if let Ok("1") = std::env::var("PURKKA_DEBUG").as_ref().map(String::as_str) {
        true
    } else {
        false
    }
}

#[cfg(not(debug_assertions))]
pub fn is_debug_enabled(_ident: DebugVal) -> bool {
    false
}

pub fn if_debug<CB>(ident: DebugVal, cb: CB)
where
    CB: FnOnce() -> (),
{
    if is_debug_enabled(ident) {
        cb();
    }
}

#[inline]
pub fn debug_p(d: DebugVal, s: &str) {
    if is_debug_enabled(d) {
        println!("{}", s);
    }
}
