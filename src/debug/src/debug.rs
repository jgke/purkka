use std::env;

pub enum DebugVal {
    IncludeName,
    DumpLalrTable,
    LalrRuntime,
    DebugFragment,
    MacroExpand,
}

pub static DEBUG_VALS: &[&str] = &[
    "DEBUG_INCLUDE_NAME",
    "DEBUG_DUMP_LALR_TABLE",
    "DEBUG_LALR_RUNTIME",
    "DEBUG_FRAGMENT",
    "DEBUG_MACRO_EXPAND",
];

pub fn is_debug_enabled(ident: DebugVal) -> bool {
    if let Ok("1") = env::var(DEBUG_VALS[ident as usize]).as_ref().map(|t| t.as_str()) {
        true
    } else {
        false
    }
}

pub fn if_debug<CB>(ident: DebugVal, cb: CB)
where
    CB: Fn() -> ()
{
    if is_debug_enabled(ident) {
        cb();
    }
}
