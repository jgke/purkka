use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use debug::debug::*;
use fragment::fragment::{FragmentIterator, Source};
use resolve::{FileQuery, ResolveResult};
use shared::intern::StringInterner;
use shared::utils::*;

use crate::calculator::eval_expression;
use crate::macrotoken::{MacroToken, MacroTokenType};
use crate::tokentype::{Operator, Punctuation, OPERATORS, PUNCTUATION};

#[derive(Debug)]
pub struct Output {
    pub macro_functions: HashMap<String, ()>,
    pub output: String,
}

#[derive(Clone, Debug)]
pub enum Macro {
    Text(Source, Vec<MacroToken>),
    Function(Source, Vec<Rc<str>>, Vec<MacroToken>, Option<Rc<str>>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MacroType {
    //Conditional macros
    If,
    Ifdef,
    Ifndef,
    Elif,
    Else,
    Endif,

    Include(bool),

    // Define function/object, undef
    Define,
    Undef,

    // Misc. macros
    Line,
    Pragma,
    Error,

    // common extension
    Warning,

    // Unknown macro
    Unknown,
}

/// Macro context.
pub struct MacroContext {
    pub symbols: HashMap<Rc<str>, Macro>,
    pub iter: Option<FragmentIterator>,
    intern: StringInterner,
}

struct InternalMacroContext<'a, CB>
where
    CB: FnMut(FileQuery) -> ResolveResult,
{
    ctx: &'a mut MacroContext,
    get_file: CB,
    if_stack: Vec<Option<bool>>,
}

pub type ParseResult<T> = Result<T, &'static str>;

impl MacroContext {
    pub(crate) fn new() -> MacroContext {
        let mut syms = (StringInterner::new(), HashMap::new());
        syms.empty("__extension__");
        syms.num("__STDC__", "1");
        syms.num("__STDC_HOSTED__", "1");
        syms.num("__GNUC__", "7");
        syms.num("__GNUC_MINOR__", "4");

        syms.num("__X86_64__", "1");
        syms.num("__LP64__", "1");

        /* common types */
        syms.ident("__SIZE_TYPE__", "long unsigned int");
        syms.ident("__PTRDIFF_TYPE__", "long int");
        syms.ident("__WCHAR_TYPE__", "int");
        syms.ident("__WINT_TYPE__", "unsigned int");
        syms.ident("__INTMAX_TYPE__", "long int");
        syms.ident("__UINTMAX_TYPE__", "long unsigned int");
        syms.ident("__SIG_ATOMIC_TYPE__", "int");
        syms.ident("__INT8_TYPE__", "signed char");
        syms.ident("__INT16_TYPE__", "short int");
        syms.ident("__INT32_TYPE__", "int");
        syms.ident("__INT64_TYPE__", "long int");
        syms.ident("__UINT8_TYPE__", "unsigned char");
        syms.ident("__UINT16_TYPE__", "short unsigned int");
        syms.ident("__UINT32_TYPE__", "unsigned int");
        syms.ident("__UINT64_TYPE__", "long unsigned int");
        syms.ident("__INT_LEAST8_TYPE__", "signed char");
        syms.ident("__INT_LEAST16_TYPE__", "short int");
        syms.ident("__INT_LEAST32_TYPE__", "int");
        syms.ident("__INT_LEAST64_TYPE__", "long int");
        syms.ident("__UINT_LEAST8_TYPE__", "unsigned char");
        syms.ident("__UINT_LEAST16_TYPE__", "short unsigned int");
        syms.ident("__UINT_LEAST32_TYPE__", "unsigned int");
        syms.ident("__UINT_LEAST64_TYPE__", "long unsigned int");
        syms.ident("__INT_FAST8_TYPE__", "signed char");
        syms.ident("__INT_FAST16_TYPE__", "long int");
        syms.ident("__INT_FAST32_TYPE__", "long int");
        syms.ident("__INT_FAST64_TYPE__", "long int");
        syms.ident("__UINT_FAST8_TYPE__", "unsigned char");
        syms.ident("__UINT_FAST16_TYPE__", "long unsigned int");
        syms.ident("__UINT_FAST32_TYPE__", "long unsigned int");
        syms.ident("__UINT_FAST64_TYPE__", "long unsigned int");
        syms.ident("__INTPTR_TYPE__", "long int");
        syms.ident("__UINTPTR_TYPE__", "long unsigned int");

        syms.ident("_Float16", "float");
        syms.ident("_Float32", "float");
        syms.ident("_Float32x", "float");
        syms.ident("_Float64", "double");
        syms.ident("_Float64x", "double");
        syms.ident("_Float128", "double");
        syms.ident("_Float128x", "double");

        syms.num("__CHAR_BIT__", "8");

        syms.num("__SCHAR_MAX__", "0x7f");
        syms.num("__WCHAR_MAX__", "0x7fffffff");
        syms.num("__SHRT_MAX__", "0x7fff");
        syms.num("__INT_MAX__", "0x7fffffff");
        syms.num("__LONG_MAX__", "0x7fffffffffffffffL");
        syms.num("__LONG_LONG_MAX__", "0x7fffffffffffffffLL");
        syms.num("__WINT_MAX__", "0xffffffffU");
        syms.num("__SIZE_MAX__", "0xffffffffffffffffUL");
        syms.num("__PTRDIFF_MAX__", "0x7fffffffffffffffL");
        syms.num("__INTMAX_MAX__", "0x7fffffffffffffffL");
        syms.num("__UINTMAX_MAX__", "0xffffffffffffffffUL");
        syms.num("__SIG_ATOMIC_MAX__", "0x7fffffff");
        syms.num("__INT8_MAX__", "0x7f");
        syms.num("__INT16_MAX__", "0x7fff");
        syms.num("__INT32_MAX__", "0x7fffffff");
        syms.num("__INT64_MAX__", "0x7fffffffffffffffL");
        syms.num("__UINT8_MAX__", "0xff");
        syms.num("__UINT16_MAX__", "0xffff");
        syms.num("__UINT32_MAX__", "0xffffffffU");
        syms.num("__UINT64_MAX__", "0xffffffffffffffffUL");
        syms.num("__INT_LEAST8_MAX__", "0x7f");
        syms.num("__INT_LEAST16_MAX__", "0x7fff");
        syms.num("__INT_LEAST32_MAX__", "0x7fffffff");
        syms.num("__INT_LEAST64_MAX__", "0x7fffffffffffffffL");
        syms.num("__UINT_LEAST8_MAX__", "0xff");
        syms.num("__UINT_LEAST16_MAX__", "0xffff");
        syms.num("__UINT_LEAST32_MAX__", "0xffffffffU");
        syms.num("__UINT_LEAST64_MAX__", "0xffffffffffffffffUL");
        syms.num("__INT_FAST8_MAX__", "0x7f");
        syms.num("__INT_FAST16_MAX__", "0x7fffffffffffffffL");
        syms.num("__INT_FAST32_MAX__", "0x7fffffffffffffffL");
        syms.num("__INT_FAST64_MAX__", "0x7fffffffffffffffL");
        syms.num("__UINT_FAST8_MAX__", "0xff");
        syms.num("__UINT_FAST16_MAX__", "0xffffffffffffffffUL");
        syms.num("__UINT_FAST32_MAX__", "0xffffffffffffffffUL");
        syms.num("__UINT_FAST64_MAX__", "0xffffffffffffffffUL");
        syms.num("__INTPTR_MAX__", "0x7fffffffffffffffL");
        syms.num("__UINTPTR_MAX__", "0xffffffffffffffffUL");
        syms.num("__WCHAR_MIN__", "(-0x7fffffff - 1)");
        syms.num("__WINT_MIN__", "0U");
        syms.num("__SIG_ATOMIC_MIN__", "(-0x7fffffff - 1)");

        syms.ident("__INT8_C", "__INT8_C");
        syms.ident("__INT16_C", "__INT16_C");
        syms.ident("__INT32_C", "__INT32_C");
        syms.ident("__INT64_C", "__INT64_C");
        syms.ident("__UINT8_C", "__UINT8_C");
        syms.ident("__UINT16_C", "__UINT16_C");
        syms.ident("__UINT32_C", "__UINT32_C");
        syms.ident("__UINT64_C", "__UINT64_C");
        syms.ident("__INTMAX_C", "__INTMAX_C");
        syms.ident("__UINTMAX_C", "__UINTMAX_C");

        syms.ident("__SCHAR_WIDTH__", "8");
        syms.ident("__SHRT_WIDTH__", "16");
        syms.ident("__INT_WIDTH__", "32");
        syms.ident("__LONG_WIDTH__", "64");
        syms.ident("__LONG_LONG_WIDTH__", "64");
        syms.ident("__PTRDIFF_WIDTH__", "64");
        syms.ident("__SIG_ATOMIC_WIDTH__", "32");
        syms.ident("__SIZE_WIDTH__", "64");
        syms.ident("__WCHAR_WIDTH__", "32");
        syms.ident("__WINT_WIDTH__", "32");
        syms.ident("__INT_LEAST8_WIDTH__", "8");
        syms.ident("__INT_LEAST16_WIDTH__", "16");
        syms.ident("__INT_LEAST32_WIDTH__", "32");
        syms.ident("__INT_LEAST64_WIDTH__", "64");
        syms.ident("__INT_FAST8_WIDTH__", "8");
        syms.ident("__INT_FAST16_WIDTH__", "64");
        syms.ident("__INT_FAST32_WIDTH__", "64");
        syms.ident("__INT_FAST64_WIDTH__", "64");
        syms.ident("__INTPTR_WIDTH__", "64");
        syms.ident("__INTMAX_WIDTH__", "64");

        syms.ident("__SIZEOF_INT__", "4");
        syms.ident("__SIZEOF_LONG__", "8");
        syms.ident("__SIZEOF_LONG_LONG__", "8");
        syms.ident("__SIZEOF_SHORT__", "2");
        syms.ident("__SIZEOF_POINTER__", "8");
        syms.ident("__SIZEOF_FLOAT__", "4");
        syms.ident("__SIZEOF_DOUBLE__", "8");
        syms.ident("__SIZEOF_LONG_DOUBLE__", "16");
        syms.ident("__SIZEOF_SIZE_T__", "8");
        syms.ident("__SIZEOF_WCHAR_T__", "4");
        syms.ident("__SIZEOF_WINT_T__", "4");
        syms.ident("__SIZEOF_PTRDIFF_T__", "8");

        syms.ident("__BYTE_ORDER__", "1234");
        syms.ident("__ORDER_LITTLE_ENDIAN__", "1234");
        syms.ident("__ORDER_BIG_ENDIAN__", "4321");
        syms.ident("__ORDER_PDP_ENDIAN__", "3412");

        syms.ident("__FLOAT_WORD_ORDER__", "1234");

        syms.ident("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1", "1");
        syms.ident("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2", "1");
        syms.ident("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4", "1");
        syms.ident("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8", "1");

        syms.ident("__GCC_IEC_559", "2");

        syms.ident("__GCC_IEC_559_COMPLEX", "2");

        MacroContext {
            intern: syms.0,
            symbols: syms.1,
            iter: None,
        }
    }

    pub(crate) fn add_definitions<CB: FnMut(FileQuery) -> ResolveResult>(&mut self, definitions: &[(&str, &str)], get_file: CB) {
        InternalMacroContext {
            ctx: self,
            get_file,
            if_stack: Vec::new(),
        }.add_definitions(definitions)
    }

    /// Divide src into MacroTokens.
    pub(crate) fn preprocess_file<CB: FnMut(FileQuery) -> ResolveResult>(&mut self, filename: &str, get_file: CB) -> Vec<MacroToken> {
        InternalMacroContext {
            ctx: self,
            get_file,
            if_stack: Vec::new(),
        }.preprocess_file(filename)
    }

    /// Divide src into MacroTokens.
    pub(crate) fn preprocess_str<CB: FnMut(FileQuery) -> ResolveResult>(&mut self, content: &str, get_file: CB) -> Vec<MacroToken> {
        InternalMacroContext {
            ctx: self,
            get_file,
            if_stack: Vec::new(),
        }.preprocess_str(content)
    }
}

impl<CB> InternalMacroContext<'_, CB>
where
    CB: FnMut(FileQuery) -> ResolveResult,
{
    pub(crate) fn add_definitions(&mut self, definitions: &[(&str, &str)]) {
        for (key, value) in definitions {
            let mut iter = FragmentIterator::new(key, value);
            let mut vals = Vec::new();
            while iter.peek().is_some() {
                let token = self.get_token(&mut iter, false);
                if let (Some(t), _) = token {
                    vals.push(MacroToken::dummy(t.ty));
                }
            }
            self.ctx.symbols
                .insert(self.ctx.intern.get_ref(key), Macro::Text(Source::dummy(), vals));
            assert!(iter.peek().is_none());
        }
    }

    /// Divide src into MacroTokens.
    pub(crate) fn preprocess_str(&mut self, content: &str) -> Vec<MacroToken> {
        let mut iter = self.get_iterator_from_str(content);
        let res = self.preprocess(&mut iter);
        self.save_iterator(Some(iter));
        res
    }

    /// Divide src into MacroTokens.
    pub(crate) fn preprocess_file(&mut self, filename: &str) -> Vec<MacroToken> {
        let mut iter = self.get_iterator_from_file(filename);
        let res = self.preprocess(&mut iter);
        self.save_iterator(Some(iter));
        res
    }

    fn preprocess(&mut self, iter: &mut FragmentIterator) -> Vec<MacroToken> {
        let mut out: Vec<MacroToken> = Vec::new();
        {
            let mut can_parse_macro = true;
            loop {
                match iter.peek() {
                    Some('#') => {
                        if can_parse_macro {
                            self.read_macro(iter);
                        } else {
                            panic!("Spurious #");
                        }
                    }
                    Some(_) => {
                        let ty = self.get_token(iter, can_parse_macro);
                        can_parse_macro = ty.1;
                        if let Some(token) = ty.0 {
                            if let Some(ident) = token.get_identifier_str() {
                                if self.is_macro(&ident) {
                                    if_debug(DebugVal::MacroExpand, || {
                                        println!("Starting expansion process for '{}'", ident);
                                    });
                                    out.extend(
                                        self.maybe_expand_identifier(token, iter).into_iter(),
                                    );
                                } else {
                                    if_debug(DebugVal::MacroExpand, || {
                                        println!("Not expanding '{}', not a macro", ident);
                                    });
                                    out.push(token);
                                }
                            } else {
                                out.push(token)
                            }
                        }
                    }
                    None => {
                        if !iter.advance_and_reset_span() {
                            break;
                        }
                    }
                }
            }
        }

        out
    }

    fn get_iterator_from_file(&mut self, filename: &str) -> FragmentIterator {
        let content = (self.get_file)(FileQuery::new(".", filename, true, true, HashSet::new()));
        let full_path = &content.full_path;
        let file_content = &content.h_content.unwrap_or(content.c_content);
        let mut iter = None;
        std::mem::swap(&mut iter, &mut self.ctx.iter);
        iter.map(|mut t| {
            // Reset the iterator by consuming all of the possibly remaining junk, then add the
            // file to the end
            while t.advance_and_reset_span() {}
            while t.next().is_some() {}
            t.split_and_push_file(full_path, file_content);
            t
        })
        .unwrap_or_else(|| FragmentIterator::new(full_path, file_content))
    }

    fn get_iterator_from_str(&mut self, content: &str) -> FragmentIterator {
        let mut iter = None;
        std::mem::swap(&mut iter, &mut self.ctx.iter);
        iter.map(|mut t| {
            // Reset the iterator by consuming all of the possibly remaining junk, then add the
            // file to the end
            while t.advance_and_reset_span() {}
            while t.next().is_some() {}
            t.split_and_push_file("<macro expansion>", content);
            t
        })
        .unwrap_or_else(|| FragmentIterator::new("<macro expansion>", content))
    }

    fn save_iterator(&mut self, mut iter: Option<FragmentIterator>) {
        assert!(self.ctx.iter.is_none());
        std::mem::swap(&mut iter, &mut self.ctx.iter);
    }


    fn get_some_token(
        &mut self,
        iter: &mut FragmentIterator,
        parse_macro: bool,
    ) -> (MacroToken, bool) {
        let res;
        'outer: loop {
            while iter.peek().is_some() {
                let tok = self.get_token(iter, parse_macro);
                if let Some(t) = tok.0 {
                    res = (t, tok.1);
                    break 'outer;
                }
            }
            panic!();
        }
        res
    }

    fn get_token(
        &mut self,
        iter: &mut FragmentIterator,
        parse_macro: bool,
    ) -> (Option<MacroToken>, bool) {
        let next = iter.peek_n(2);
        match next.as_ref() {
            "//" => {
                self.preprocess_get_macro_line(iter, false, true);
                (None, true)
            }
            "/*" => {
                iter.next(); // /
                iter.next(); // *
                self.preprocess_flush_until('*', '/', iter, false);
                (None, parse_macro)
            }
            "\\\n" => {
                iter.next(); // \
                iter.next(); // \n
                (None, parse_macro)
            }
            _ => match iter.peek() {
                Some('"') => (Some(self.read_string(iter)), false),
                Some('\'') => (Some(self.read_char(iter)), false),
                Some('.') => {
                    let substr = iter.peek_n(2);
                    let mut substr_iter = substr.chars();
                    substr_iter.next();
                    let token = if let Some(c) = substr_iter.next() {
                        match c {
                            '0'..='9' => self.read_number(iter),
                            _ => self.read_other(iter),
                        }
                    } else {
                        self.read_other(iter)
                    };
                    (Some(token), false)
                }
                Some(' ') => {
                    iter.next();
                    (None, parse_macro)
                }
                Some('\t') => {
                    iter.next();
                    (None, parse_macro)
                }
                Some('\n') => {
                    iter.next();
                    (None, true)
                }
                Some(c) => {
                    let token = match c {
                        'a'..='z' | 'A'..='Z' | '_' => self.read_identifier(iter),
                        '0'..='9' => self.read_number(iter),
                        _ => self.read_other(iter),
                    };
                    (Some(token), false)
                }
                None => (None, parse_macro),
            },
        }
    }

    fn preprocess_flush_until(
        &self,
        c1: char,
        c2: char,
        iter: &mut FragmentIterator,
        preserve_last: bool,
    ) {
        while iter.peek().is_some() {
            if iter.next() == Some(c1) && iter.peek() == Some(c2) {
                break;
            }
        }
        if !preserve_last {
            match iter.next() {
                Some(_) => {}
                None => panic!("Unexpected end of file"),
            }
        }
    }

    fn preprocess_get_macro_line(
        &self,
        iter: &mut FragmentIterator,
        parse_comments: bool,
        start_new_span: bool,
    ) -> (Vec<(usize, char)>, Source) {
        let lo = iter.current_source().span.lo;
        if iter.peek() == Some('\n') {
            let val = iter.next_new_span().unwrap();
            let source = iter.current_source();
            return (vec![(source.span.lo, val)], source);
        }
        let (s, mut src) = iter.collect_while_flatmap(|c, i| match c {
            '\\' => {
                i.next();
                match i.peek() {
                    Some('\n') => Some(vec![' ']),
                    Some(c) => Some(vec!['\\', c]),
                    _ => panic!("\\ at end of file"),
                }
            }
            '\n' => {
                i.next();
                None
            }
            '\t' => Some(vec![' ']),
            c => {
                if c == '/' && parse_comments {
                    let next = i.peek_n(2);
                    match next.as_ref() {
                        "//" => {
                            self.preprocess_get_macro_line(i, false, false);
                            None
                        }
                        "/*" => {
                            i.next(); // /
                            i.next(); // *
                            self.preprocess_flush_until('*', '/', i, true);
                            Some(vec![])
                        }
                        _ => Some(vec![c]),
                    }
                } else {
                    Some(vec![c])
                }
            }
        });
        if !start_new_span {
            src.span.lo = lo;
        }
        (s, src)
    }

    fn read_identifier(&mut self, iter: &mut FragmentIterator) -> MacroToken {
        let (identifier, source) = self.read_identifier_raw(iter);
        MacroToken {
            source,
            ty: MacroTokenType::Identifier(identifier),
        }
    }

    fn read_identifier_raw(&mut self, iter: &mut FragmentIterator) -> (Rc<str>, Source) {
        let (ident, src) = iter.collect_while(|c| match c {
            '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => true,
            _ => false,
        });
        (self.ctx.intern.get_ref(&ident), src)
    }

    fn read_number(&mut self, iter: &mut FragmentIterator) -> MacroToken {
        let exponents = &["e+", "e-", "E+", "E-", "p+", "p-", "P+", "P-"];
        let (number, source) = iter.collect_while_flatmap(|c, i| {
            exponents
                .iter()
                .filter(|e| i.starts_with(e))
                .last()
                .map(|e| {
                    i.next();
                    Some(e.chars().collect())
                })
                .unwrap_or_else(|| match c {
                    'a'..='z' | 'A'..='Z' => Some(vec![c]),
                    '0'..='9' => Some(vec![c]),
                    '_' => Some(vec![c]),
                    '.' => Some(vec![c]),
                    _ => None,
                })
        });
        MacroToken {
            source,
            ty: MacroTokenType::Number(
                self.ctx.intern
                    .get_ref(&number.iter().map(|t| t.1).collect::<String>()),
            ),
        }
    }

    fn read_string(&mut self, iter: &mut FragmentIterator) -> MacroToken {
        match iter.next_new_span() {
            Some('"') => {}
            Some(t) => panic!("Unexpected character: {}", t),
            None => panic!("Unexpected end of file"),
        }

        let mut content = String::new();

        loop {
            match iter.next() {
                Some('"') => {
                    break;
                }
                Some('\n') => panic!("Missing terminating \" character"),
                Some('\\') => {
                    if let Some(c) = iter.peek() {
                        content.push(self.read_string_escape(iter, c));
                    } else {
                        panic!("Unexpected end of file");
                    }
                }
                Some(c) => content.push(c),
                None => panic!("Unexpected end of file"),
            }
        }

        MacroToken {
            source: iter.current_source(),
            ty: MacroTokenType::StringLiteral(self.ctx.intern.get_ref(&content)),
        }
    }

    fn read_char(&mut self, iter: &mut FragmentIterator) -> MacroToken {
        match iter.next_new_span() {
            Some('\'') => {}
            Some(t) => panic!("Unexpected character: {}", t),
            None => panic!("Unexpected end of file"),
        }
        let c = match iter.next() {
            Some('\\') => {
                if let Some(c) = iter.peek() {
                    self.read_string_escape(iter, c)
                } else {
                    panic!("Unexpected end of file");
                }
            }
            Some('\n') => panic!("Unexpected newline"),
            Some(c) => c,
            None => panic!("Unexpected end of file"),
        };
        match iter.next() {
            Some('\'') => {}
            Some(t) => panic!("Unexpected character: {}", t),
            None => panic!("Unexpected end of file"),
        }
        MacroToken {
            source: iter.current_source(),
            ty: MacroTokenType::Char(c),
        }
    }

    fn read_string_escape(&self, iter: &mut FragmentIterator, c: char) -> char {
        let ret = match c {
            '\'' => '\'',
            '"' => '\"',
            '?' => '?',
            '\\' => '\\',
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0C',
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            'v' => '\x0B',
            c @ '0'..='7' => return self.get_octal(iter, c),
            'x' => return self.get_hex(iter),
            c => c,
        };
        iter.next();
        ret
    }

    fn get_octal(&self, iter: &mut FragmentIterator, c: char) -> char {
        iter.next();
        match iter.peek() {
            Some(second @ '0'..='7') => {
                iter.next();
                match iter.peek() {
                    Some(third @ '0'..='7') => {
                        iter.next();
                        char_from_octal(c, second, third)
                    }
                    _ => char_from_octal('0', c, second),
                }
            }
            _ => char_from_octal('0', '0', c),
        }
    }

    #[allow(clippy::char_lit_as_u8)]
    fn get_hex(&self, iter: &mut FragmentIterator) -> char {
        let mut num: u8 = 0;
        while let Some(c) = iter.peek() {
            match c {
                c @ '0'..='9' => {
                    num = num.saturating_mul(16).saturating_add(num_val(c));
                    iter.next();
                }
                c @ 'a'..='f' => {
                    num = num
                        .saturating_mul(16)
                        .saturating_add(c as u8 - 'a' as u8 + 10);
                    iter.next();
                }
                c @ 'A'..='F' => {
                    num = num
                        .saturating_mul(16)
                        .saturating_add(c as u8 - 'A' as u8 + 10);
                    iter.next();
                }
                _ => break,
            }
        }
        num as char
    }

    fn read_other(&self, iter: &mut FragmentIterator) -> MacroToken {
        for (punctuation, p) in PUNCTUATION.iter() {
            if iter.starts_with(punctuation) {
                iter.next_new_span();
                for _ in 1..punctuation.len() {
                    iter.next();
                }
                return MacroToken {
                    source: iter.current_source(),
                    ty: MacroTokenType::Punctuation(**p),
                };
            }
        }

        for (operator, op) in OPERATORS.iter() {
            if iter.starts_with(operator) {
                iter.next_new_span();
                for _ in 1..operator.len() {
                    iter.next();
                }
                return MacroToken {
                    source: iter.current_source(),
                    ty: MacroTokenType::Operator(**op),
                };
            }
        }

        match iter.next_new_span() {
            Some(c) => MacroToken {
                source: iter.current_source(),
                ty: MacroTokenType::Other(c),
            },
            _ => unreachable!(),
        }
    }

    fn flush_whitespace(&mut self, iter: &mut FragmentIterator) {
        loop {
            match iter.peek() {
                Some(' ') | Some('\t') => iter.next(),
                _ => break,
            };
        }
    }

    /// Parse a macro (eg. #define FOO BAR) and store it for later, or act instantly in case of #if
    fn read_macro(&mut self, iter: &mut FragmentIterator) {
        let (ty, mut sub_iter, total_span) = self.get_macro_type(iter);
        match ty {
            MacroType::Define => {
                let (left, _) = self.read_identifier_raw(&mut sub_iter);
                if let Some('(') = sub_iter.peek() {
                    assert_eq!(
                        self.get_token(&mut sub_iter, false).0.map(|x| x.ty),
                        Some(MacroTokenType::Punctuation(Punctuation::OpenParen))
                    );
                    let mut args = Vec::new();
                    let mut had_comma = None;
                    let mut varargs = false;
                    let mut varargs_str: Option<Rc<str>> = None;
                    while sub_iter.peek().is_some() {
                        if let (Some(token), _) = self.get_token(&mut sub_iter, false) {
                            match (varargs, had_comma, token.ty.clone()) {
                                (_, None, MacroTokenType::Punctuation(Punctuation::CloseParen)) => {
                                    break
                                }
                                (
                                    _,
                                    Some(false),
                                    MacroTokenType::Punctuation(Punctuation::CloseParen),
                                ) => break,
                                (false, None, MacroTokenType::Identifier(ident)) => {
                                    args.push(ident);
                                    had_comma = Some(false);
                                }
                                (false, Some(true), MacroTokenType::Identifier(ident)) => {
                                    args.push(ident);
                                    had_comma = Some(false);
                                }
                                (
                                    false,
                                    Some(false),
                                    MacroTokenType::Punctuation(Punctuation::Comma),
                                ) => {
                                    had_comma = Some(true);
                                }
                                (
                                    false,
                                    Some(false),
                                    MacroTokenType::Punctuation(Punctuation::Varargs),
                                ) => {
                                    varargs = true;
                                    varargs_str = Some(args.pop().unwrap());
                                }
                                (false, _, MacroTokenType::Punctuation(Punctuation::Varargs)) => {
                                    varargs = true;
                                    varargs_str = Some(self.ctx.intern.get_ref("__VA_ARGS__"));
                                    had_comma = Some(false);
                                }
                                _ => panic!(
                                    "Unexpected token: {:?}\n{}",
                                    token.ty,
                                    iter.source_to_str(&token.source)
                                ),
                            }
                        }
                    }

                    let mut right = Vec::new();

                    while sub_iter.peek().is_some() {
                        if let (Some(token), _) = self.get_token(&mut sub_iter, false) {
                            right.push(token);
                        }
                    }

                    self.ctx.symbols
                        .insert(left, Macro::Function(total_span, args, right, varargs_str));
                } else {
                    let mut right = Vec::new();

                    while sub_iter.peek().is_some() {
                        if let (Some(token), _) = self.get_token(&mut sub_iter, false) {
                            right.push(token);
                        }
                    }

                    self.ctx.symbols.insert(left, Macro::Text(total_span, right));
                }
            }
            MacroType::Include(next) => {
                loop {
                    match sub_iter.peek() {
                        Some(' ') | Some('\t') => sub_iter.next(),
                        _ => break,
                    };
                }
                let start = self.read_other(&mut sub_iter);
                let is_quote = if let MacroTokenType::Other('"') = start.ty {
                    true
                } else if let MacroTokenType::Operator(Operator::LessThan) = start.ty {
                    false
                } else {
                    panic!("Unexpected character: {}", start.display(&sub_iter));
                };

                let (filename, _) =
                    sub_iter.collect_while(|x| (is_quote && x != '"') || (!is_quote && x != '>'));

                let end = self.read_other(&mut sub_iter);
                if let MacroTokenType::Other('"') = end.ty {
                    if !is_quote {
                        panic!("Invalid closing character")
                    }
                } else if let MacroTokenType::Operator(Operator::MoreThan) = end.ty {
                    if is_quote {
                        panic!("Invalid closing character")
                    }
                } else {
                    panic!("Unexpected character: {}", end.display(&sub_iter));
                };

                let content = (self.get_file)(FileQuery::new_next(
                    &iter.current_filename(),
                    &filename,
                    is_quote,
                    true,
                    next,
                    HashSet::new(),
                ));
                iter.split_and_push_file(
                    &content.full_path,
                    &content.h_content.unwrap_or(content.c_content),
                );
            }
            MacroType::Undef => {
                let (left, _) = self.read_identifier_raw(&mut sub_iter);
                self.ctx.symbols.remove(&left);
            }
            MacroType::If => self.handle_if(iter, &mut sub_iter),
            MacroType::Elif => self.handle_elif(iter, &mut sub_iter),
            MacroType::Ifdef | MacroType::Ifndef => {
                self.handle_ifdef(iter, &mut sub_iter, ty == MacroType::Ifndef)
            }
            MacroType::Else => self.handle_else(iter),
            MacroType::Endif => self.handle_endif(),
            MacroType::Error => panic!("#error: {}\n{:?}", sub_iter.as_str(), total_span),
            MacroType::Pragma => println!("Ignoring #pragma: {}", sub_iter.as_str()),

            ty => unimplemented!("{:?} not implemented", ty),
        }
    }

    fn get_macro_type(
        &mut self,
        iter: &mut FragmentIterator,
    ) -> (MacroType, FragmentIterator, Source) {
        let (row_string, total_span) = self.preprocess_get_macro_line(iter, true, true);
        let mut sub_iter = FragmentIterator::with_offset(
            &iter.current_filename(),
            row_string,
            total_span.span.lo,
            &iter,
        );
        assert_eq!(sub_iter.peek(), Some('#'));
        assert_eq!(
            self.get_some_token(&mut sub_iter, false).0.ty,
            MacroTokenType::Operator(Operator::Macro)
        );
        self.flush_whitespace(&mut sub_iter);
        let (ty, _) = self.read_identifier_raw(&mut sub_iter);
        let macro_type = match ty.as_ref() {
            "if" => MacroType::If,
            "ifdef" => MacroType::Ifdef,
            "ifndef" => MacroType::Ifndef,
            "elif" => MacroType::Elif,
            "else" => MacroType::Else,
            "endif" => MacroType::Endif,
            "include" => MacroType::Include(false),
            "include_next" => MacroType::Include(true),
            "define" => MacroType::Define,
            "undef" => MacroType::Undef,
            "line" => MacroType::Line,
            "pragma" => MacroType::Pragma,
            "error" => MacroType::Error,
            "warning" => MacroType::Warning,
            _ => MacroType::Unknown,
        };
        (macro_type, sub_iter, total_span)
    }

    fn get_if_macro_type_f(&self, row: &[(usize, char)]) -> MacroType {
        let s = row
            .iter()
            .map(|(_, c)| *c)
            .skip(1) // '#'
            .skip_while(|c| *c == ' ' || *c == '\t')
            .take(6)
            .map(|c| if c == '\t' { ' ' } else { c })
            .collect::<String>();

        if s.starts_with("if ") || s == "if" {
            MacroType::If
        } else if s.starts_with("ifdef ") || s == "ifdef" {
            MacroType::Ifdef
        } else if s.starts_with("ifndef ") || s == "ifndef" {
            MacroType::Ifndef
        } else if s.starts_with("elif ") || s == "elif" {
            MacroType::Elif
        } else if s.starts_with("else ") || s == "else" {
            MacroType::Else
        } else if s.starts_with("endif ") || s == "endif" {
            MacroType::Endif
        } else {
            MacroType::Unknown
        }
    }

    fn evaluate_if(&mut self, sub_iter: &mut FragmentIterator) -> bool {
        let mut tokens = vec![];
        while sub_iter.peek().is_some() {
            if let Some(token) = self.get_token(sub_iter, false).0 {
                if token.get_identifier_str().map(|t| t.as_ref() == "defined") == Some(true) {
                    let next = self.get_some_token(sub_iter, false).0;
                    let (ident, source) =
                        if next.ty == MacroTokenType::Punctuation(Punctuation::OpenParen) {
                            let ident_token = self.get_some_token(sub_iter, false).0;
                            let tmp_ident = ident_token.get_identifier_str().unwrap();
                            let close_paren = self.get_some_token(sub_iter, false).0.ty;
                            assert!(
                                close_paren == MacroTokenType::Punctuation(Punctuation::CloseParen)
                            );
                            (tmp_ident, ident_token.source)
                        } else {
                            let tmp_ident = next.get_identifier_str().unwrap();
                            (tmp_ident, next.source)
                        };
                    let tok = if self.ctx.symbols.contains_key(&ident) {
                        MacroToken {
                            source,
                            ty: MacroTokenType::Number(self.ctx.intern.get_ref("1")),
                        }
                    } else {
                        MacroToken {
                            source,
                            ty: MacroTokenType::Number(self.ctx.intern.get_ref("0")),
                        }
                    };
                    tokens.push(tok);
                } else {
                    // Undefined behaviour: defined() appearing as the result of an expansion.
                    // Resolution: pass it forward as an identifier, for simplicity. GCC resolves
                    // it 'normally'. Probably change later to follow GCC's example.
                    let expanded = self.maybe_expand_identifier(token, sub_iter);
                    tokens.extend(expanded)
                }
            }
        }
        for token in &mut tokens {
            if token.get_identifier_str().is_some() {
                token.ty = MacroTokenType::Number(self.ctx.intern.get_ref("0"));
            }
        }

        // Actual evaluation part.
        eval_expression(&tokens)
    }

    fn handle_if(&mut self, iter: &mut FragmentIterator, sub_iter: &mut FragmentIterator) {
        let value = self.evaluate_if(sub_iter);

        self.if_stack.push(Some(value));

        if !value {
            self.skip_lines_to_else_or_endif(iter, true);
        }
    }

    fn handle_elif(&mut self, iter: &mut FragmentIterator, sub_iter: &mut FragmentIterator) {
        let popped = self.if_stack.pop();
        match popped {
            Some(Some(false)) => {
                let value = self.evaluate_if(sub_iter);
                self.if_stack.push(Some(value));

                if !value {
                    self.skip_lines_to_else_or_endif(iter, true);
                }
            }
            Some(Some(true)) => {
                self.if_stack.push(Some(true));
                self.skip_lines_to_else_or_endif(iter, true);
            }
            Some(None) => panic!("Cannot handle #elif after #else"),
            None => panic!("Spurious #else"),
        }
    }

    fn handle_ifdef(
        &mut self,
        iter: &mut FragmentIterator,
        sub_iter: &mut FragmentIterator,
        is_ifndef: bool,
    ) {
        loop {
            match sub_iter.peek() {
                Some(' ') | Some('\t') => sub_iter.next(),
                _ => break,
            };
        }

        let token = self.read_identifier(sub_iter);
        let ident = token.get_identifier_str().unwrap();

        let truth_value = is_ifndef ^ (self.ctx.symbols.get(&ident).is_some());

        self.if_stack.push(Some(truth_value));

        if !truth_value {
            self.skip_lines_to_else_or_endif(iter, true);
        }
    }

    fn flush_until_pound_or_newline(&self, iter: &mut FragmentIterator) {
        'outer: loop {
            match iter.peek() {
                Some('#') | Some('\n') => return,
                Some(' ') | Some('\t') => {
                    iter.next();
                    continue 'outer;
                }
                Some(_) => {
                    loop {
                        let next = iter.peek_n(2);
                        match (next.as_ref(), iter.peek()) {
                            ("//", _) => {
                                self.preprocess_get_macro_line(iter, false, false);
                                continue 'outer;
                            }
                            ("/*", _) => {
                                iter.next(); // /
                                iter.next(); // *
                                self.preprocess_flush_until('*', '/', iter, false);
                                continue 'outer;
                            }
                            (_, Some('\n')) => {
                                iter.next();
                                continue 'outer;
                            }
                            (_, None) => return,
                            _ => {}
                        };
                        iter.next();
                    }
                }
                None => return,
            }
        }
    }

    fn skip_lines_to_else_or_endif(&mut self, iter: &mut FragmentIterator, accept_else: bool) {
        let mut skip_to = 0;
        while iter.peek().is_some() {
            self.flush_until_pound_or_newline(iter);
            let (next_row, line_src) = self.preprocess_get_macro_line(iter, true, true);
            if let Some('#') = next_row.get(0).map(|(_, c)| c) {
                match (skip_to, accept_else, self.get_if_macro_type_f(&next_row)) {
                    (0, true, MacroType::Elif) => {
                        let mut sub_iter = self
                            .get_macro_type(&mut FragmentIterator::with_offset(
                                &iter.current_filename(),
                                next_row,
                                line_src.span.lo,
                                &iter,
                            ))
                            .1;
                        self.handle_elif(iter, &mut sub_iter);
                        return;
                    }
                    (0, true, MacroType::Else) => {
                        self.handle_else(iter);
                        return;
                    }
                    (0, false, MacroType::Else) => panic!(),
                    (0, _, MacroType::Endif) => {
                        self.handle_endif();
                        return;
                    }
                    (_, _, MacroType::If)
                    | (_, _, MacroType::Ifdef)
                    | (_, _, MacroType::Ifndef) => {
                        skip_to += 1;
                    }
                    (_, _, MacroType::Endif) => {
                        skip_to -= 1;
                    }
                    _ => {}
                };
            }
        }
        if iter.peek().is_none() {
            panic!("Unexpected eof");
        }
    }

    fn handle_else(&mut self, iter: &mut FragmentIterator) {
        let popped = self.if_stack.pop();
        if let Some(Some(truth_value)) = popped {
            self.if_stack.push(None);
            if truth_value {
                self.skip_lines_to_else_or_endif(iter, false);
            }
        } else if let Some(None) = popped {
            panic!("Cannot handle #else after another #else")
        } else {
            panic!("Spurious #else");
        }
    }

    fn handle_endif(&mut self) {
        // line has been consumed already here, so just handle the if stack
        assert!(self.if_stack.pop().is_some());
    }

    fn is_macro(&self, s: &str) -> bool {
        self.ctx.symbols.contains_key(s)
    }

    /// Expand all macros from the argument list. Consumes tokens from `iter` in case of function
    /// macros. Does not consume tokens past fragment boundaries.
    ///
    /// The implementation is based on Dave Prosser's C Preprocessing Algorithm, available at
    /// https://www.spinellis.gr/blog/20060626/ but augmented with the GCC's implementation
    /// specific behaviour as documented at
    /// https://gcc.gnu.org/onlinedocs/cppinternals/Macro-Expansion.html. The notation : indicates
    /// list construction (ie. 1 : [] == list(1) == [] : 1)
    ///
    /// Algorithm:
    /// ```text
    /// expand(TS: token-sequence, CTX: expansion-context) {
    ///     if TS is empty, then
    ///         return [];
    ///     else if TS is T : TS' and T is pop-context, then
    ///         pop CTX
    ///         return expand(TS', CTX);
    ///     else if TS is T : TS' and T is disabled, then
    ///         return T : expand(TS', CTX);
    ///     else if TS is T : TS' and T an object macro and in CTX, then
    ///         disable T
    ///         return T : expand(TS', CTX);
    ///     else if TS is T : '(' : TS' and T an function macro and in CTX, then
    ///         disable T
    ///         return T : expand(TS', CTX);
    ///     else if TS is T:TS' and T is an object macro, then
    ///         let substituted = substitute(ts(T), [], CTX, [])
    ///         add T to CTX
    ///         return expand(substituted : pop-context : TS', CTX);
    ///     else if TS is T : '(' : TS' and T is a function macro, then
    ///         let function-arguments : ')' : TS'' = TS'
    ///         let substituted = substitute(ts(T), fp(T), function-arguments, CTX, [])
    ///         add T to CTX
    ///         return expand(substituted : pop-context : :TS'', CTX);
    ///     else TS is T : TS', and
    ///         return T : expand(TS', CTX);
    /// }
    ///
    /// substitute(IS: input-sequence, FP: function-parameters,
    ///            AP: function-arguments, CTX: expansion-context,
    ///            OS: output-sequence) {
    ///     if IS is empty, then
    ///         return OS;
    ///     else if IS is pop-context : IS', then
    ///         pop CTX
    ///         return substitute(IS', FP, AP, CTX, OS);
    ///     else if IS is '#' : T : IS' and T is FP[i], then
    ///         return substitute(IS', FP, AP, OS : stringize(AP[i]));
    ///     else if IS is '##' : T : IS' and T is FP[i], then
    ///         if AP[i] is empty, then
    ///             return substitute(IS', FP, AP, CTX, OS);
    ///         else
    ///             return substitute(IS', FP, AP, CTX, glue(OS, AP[i]));
    ///     else if IS is '##' : T : IS', then
    ///         return substitute(IS', FP, AP, glue(OS, T));
    ///     else if IS is T : '##' : IS' and T is FP[i], then
    ///         if AP[i] is empty, then
    ///             if IS' is T' : IS'' and T' is FP[j], then
    ///                 return substitute(IS'', FP, AP, CTX, OS : AP[j]);
    ///             else
    ///                 return substitute(IS', FP, AP, CTX, OS);
    ///         else
    ///             return substitute('##' : IS', FP, AP, CTX, OS : AP[i]);
    ///     else if IS is T : IS' and T is FP[i], then
    ///         return substitute(IS', FP, AP, CTX, OS : expand(AP[i], CTX));
    ///     else IS is T : IS', and
    ///         return substitute(IS', FP, AP, CTX, OS : T);
    /// }
    ///
    /// glue(LS: token-sequence, RS: token-sequence) {
    ///     if LS is ',' : [] and RS is empty, then
    ///         return [];
    ///     else if LS is ',' : [], then
    ///         return LS : RS
    ///     if LS is L : [] and RS is R : RS', then
    ///         combine L and R as T
    ///         T.HS = intersection of L.HS and R.HS
    ///         return T:RS';
    ///     else LS is L : LS', and
    ///         return L : glue(LS', RS);
    /// }
    /// ```
    fn maybe_expand_identifier(
        &mut self,
        token: MacroToken,
        iter: &mut FragmentIterator,
    ) -> Vec<MacroToken> {
        let ident = token.get_identifier_str().clone();
        let mut expansion = vec![(token, true)];
        let mut ctx = ExpansionHistory::new();

        expansion = self.expand(&expansion, &mut ctx, &mut Some(iter));

        let result: Vec<MacroToken> = expansion.into_iter().map(|t| t.0).collect();
        if_debug(DebugVal::MacroExpand, || {
            if let Some(s) = ident {
                println!(
                    "Expanded {} to {}",
                    s,
                    result.iter().map(|t| format!("{} ", t)).collect::<String>()
                );
            }
        });
        result
    }

    fn pop_context_token(&self) -> (MacroToken, bool) {
        (
            MacroToken {
                ty: MacroTokenType::PopContext,
                source: Source::dummy(),
            },
            true,
        )
    }

    // These recursive functions have every recursive call in tail position, and _should_ be
    // optimized nicely by the compiler.
    //
    // The expand() function is ordered slightly differently compared to the original algorithm.
    fn expand(
        &mut self,
        input: &[(MacroToken, bool)],
        ctx: &mut ExpansionHistory,
        iter: &mut Option<&mut FragmentIterator>,
    ) -> Vec<(MacroToken, bool)> {
        if_debug(DebugVal::MacroExpand, || {
            println!(
                "Expanding {}\n{:?}",
                input
                    .iter()
                    .map(|t| format!("{} ", t.0))
                    .collect::<String>(),
                &ctx.history
            )
        });

        // else if TS is T : TS' and T is pop-context, then
        //     pop ctx-stack
        //     return expand(TS');
        let input = self.flush_pop_context(input, ctx);

        // if TS is empty, then
        //    return [];
        if input.is_empty() {
            return vec![];
        }

        let (t0, rest) = input.split_first().unwrap();
        let mut t = t0.clone();

        if_debug(DebugVal::MacroExpand, || {
            println!("\nExpansion scanning {} [{:?}]", &t.0, t.1)
        });

        if t.1 {
            match t
                .0
                .get_identifier_str()
                .as_ref()
                .and_then(|s| self.ctx.symbols.get(s).cloned().map(|t| (s, t)))
            {
                // else if TS is T:TS' and T is an object macro, then
                //    let substituted = substitute(ts(T), [], [])
                //    add T to ctx-stack
                //    return expand(substituted : pop-context :  TS', ctx);
                Some((ident, Macro::Text(_span, body))) => {
                    if !ctx.contains(ident) {
                        let converted_body = body
                            .clone()
                            .into_iter()
                            .map(|t| (t, true))
                            .collect::<Vec<_>>();
                        let mut substituted =
                            self.substitute(&converted_body, iter, HashMap::new(), ctx, Vec::new());
                        ctx.insert(t.0.get_identifier_str().unwrap());
                        substituted.push(self.pop_context_token());
                        substituted.append(&mut rest.to_vec());
                        return self.expand(&substituted, ctx, iter);
                    } else {
                        if_debug(DebugVal::MacroExpand, || {
                            println!("Not expanding, macro in context");
                        });
                        // else if TS is T : TS' and T an object macro and in ctx, then
                        //     disable T
                        //     return T : expand(TS');
                        t.1 = false;
                    }
                }
                // else if TS is T : '(' : TS' and T is a function macro, then
                Some((ident, Macro::Function(_span, args, body, varargs))) => {
                    if self.next_is_open_paren(rest, iter) {
                        let rest = self.flush_pop_context(rest, ctx);
                        if !ctx.contains(ident) {
                            let converted_body = body
                                .clone()
                                .into_iter()
                                .map(|t| (t, true))
                                .collect::<Vec<_>>();
                            // let function-arguments : ')' : TS'' = TS'
                            // let substituted = substitute(ts(T), fp(T), function-arguments, [])
                            // add T to ctx-stack
                            // return expand(substituted : pop-context : TS'', ctx);

                            let (rest, args) =
                                self.parse_arguments(rest, iter, &args, varargs.as_ref());
                            let mut substituted =
                                self.substitute(&converted_body, iter, args, ctx, Vec::new());
                            ctx.insert(t.0.get_identifier_str().unwrap());
                            substituted.push(self.pop_context_token());
                            substituted.append(&mut rest.to_vec());
                            return self.expand(&substituted, ctx, iter);
                        } else {
                            if_debug(DebugVal::MacroExpand, || {
                                println!("Not expanding, macro in context");
                            });
                            // else if TS is T : '(' : TS' and T an function macro and in ctx, then
                            //     disable T
                            //     return T : expand(TS');
                            t.1 = false;
                        }
                    } else {
                        if_debug(DebugVal::MacroExpand, || {
                            println!("Not expanding nor disabling, not followed by open paren");
                        });
                    }
                }
                None => {}
            }
        }

        // else if TS is T : TS' and T is disabled, then
        //    return T : expand(TS');
        // else TS is T : TS', and
        //    return T : expand(TS');
        let mut out = vec![t];
        out.append(&mut self.expand(rest, ctx, iter));
        out
    }

    fn substitute(
        &mut self,
        input: &[(MacroToken, bool)],
        iter: &mut Option<&mut FragmentIterator>,
        args: HashMap<Rc<str>, Vec<(MacroToken, bool)>>,
        ctx: &mut ExpansionHistory,
        mut output: Vec<(MacroToken, bool)>,
    ) -> Vec<(MacroToken, bool)> {
        if_debug(DebugVal::MacroExpand, || {
            println!(
                "Substituting {}",
                input
                    .iter()
                    .map(|t| format!("{} ", t.0))
                    .collect::<String>()
            )
        });
        if_debug(DebugVal::MacroExpand, || {
            if !args.is_empty() {
                print!("Current arguments:");
                for (arg, val) in &args {
                    print!(
                        " {}: {}\n                  ",
                        arg,
                        val.iter().map(|t| format!("{} ", t.0)).collect::<String>()
                    );
                }
                println!();
            }
        });
        if_debug(DebugVal::MacroExpand, || {
            println!(
                "Current output: {}",
                output
                    .iter()
                    .map(|t| format!("{} ", t.0))
                    .collect::<String>()
            )
        });
        // if IS is empty, then
        //    return OS;
        if input.is_empty() {
            return output;
        }

        match (&input[0].0.ty, input.get(1).as_ref().map(|t| &t.0.ty)) {
            // else if IS is pop-context : IS', then
            //     pop CTX
            //     return substitute(IS', FP, AP, CTX, OS);
            (MacroTokenType::PopContext, _) => {
                let (_, rest) = input.split_first().unwrap();
                ctx.pop_context();
                self.substitute(rest, iter, args, ctx, output)
            }
            // else if IS is '#' : T : IS' and T is FP[i], then
            //    return substitute(IS', FP, AP, CTX, OS : stringize(AP[i]));
            (
                MacroTokenType::Operator(Operator::Macro),
                Some(MacroTokenType::Identifier(ident)),
            ) if args.contains_key(ident) => {
                let (_, rest_0) = input.split_first().unwrap();
                let (_, rest) = rest_0.split_first().unwrap();
                let replacement = &args[ident];
                let stringified = replacement
                    .iter()
                    .map(|t| t.0.to_string())
                    .collect::<String>();
                let tok = MacroToken {
                    source: Source::dummy(),
                    ty: MacroTokenType::StringLiteral(From::from(stringified)),
                };
                output.push((tok, true));
                self.substitute(rest, iter, args, ctx, output)
            }
            // else if IS is '##' : T : IS' and T is FP[i], then
            (
                MacroTokenType::Operator(Operator::MacroPaste),
                Some(MacroTokenType::Identifier(ident)),
            ) if args.contains_key(ident) => {
                let replacement = args[ident].clone();
                let (_, rest_0) = input.split_first().unwrap();
                let (_, rest) = rest_0.split_first().unwrap();
                // if AP[i] is empty, then
                if replacement.is_empty()
                    && output.get(0).as_ref().map(|t| &t.0.ty)
                        != Some(&MacroTokenType::Punctuation(Punctuation::Comma))
                {
                    // return substitute(IS', FP, AP, CTX, OS);
                    self.substitute(rest, iter, args, ctx, output)
                } else {
                    // return substitute(IS', FP, AP, CTX, glue(OS, AP[i]));
                    let glued = self.glue(iter, output, replacement);
                    self.substitute(rest, iter, args, ctx, glued)
                }
            }
            // else if IS is '##' : T : IS', then
            //     return substitute(IS', FP, AP, CTX, glue(OS, T));
            (MacroTokenType::Operator(Operator::MacroPaste), Some(_)) => {
                let (_, rest_1) = input.split_first().unwrap();
                let (t0, rest) = rest_1.split_first().unwrap();
                let t = t0.clone();
                let glued = self.glue(iter, output, vec![t]);
                self.substitute(rest, iter, args, ctx, glued)
            }
            (
                MacroTokenType::Identifier(ident),
                Some(MacroTokenType::Operator(Operator::MacroPaste)),
            ) if args.contains_key(ident) => {
                let replacement = args[ident].clone();
                let (_, rest_0) = input.split_first().unwrap();
                let (_, rest) = rest_0.split_first().unwrap();
                // if AP[i] is empty, then
                if replacement.is_empty() {
                    match rest.get(0).as_ref().map(|t| &t.0.ty) {
                        // if IS' is T' : IS'' and T' is FP[j], then
                        //    return substitute(IS'', FP, AP, CTX, OS : AP[j]);
                        Some(MacroTokenType::Identifier(ident)) if args.contains_key(ident) => {
                            let (_, rest) = rest.split_first().unwrap();
                            output.append(&mut args[ident].clone());
                            self.substitute(rest, iter, args, ctx, output)
                        }
                        // else
                        //    return substitute(IS', FP, AP, CTX, OS);
                        _ => self.substitute(rest, iter, args, ctx, output),
                    }
                } else {
                    // return substitute('##' : IS', FP, AP, CTX, OS : expand(AP[i], CTX));
                    let (_, rest) = input.split_first().unwrap();
                    let mut expanded = self.expand(&args[ident].clone(), ctx, &mut None);
                    output.append(&mut expanded);
                    self.substitute(rest, iter, args, ctx, output)
                }
            }
            // else if IS is T : IS' and T is FP[i], then
            //     return substitute(IS', FP, AP, CTX, OS : expand(AP[i], CTX));
            (MacroTokenType::Identifier(ident), _) if args.contains_key(ident) => {
                let (_, rest) = input.split_first().unwrap();
                let mut expanded = self.expand(&args[ident].clone(), ctx, &mut None);
                output.append(&mut expanded);
                self.substitute(rest, iter, args, ctx, output)
            }
            // else IS is T : IS', and
            //     return substitute(IS', FP, AP, CTX, OS : T);
            _ => {
                let (t0, rest) = input.split_first().unwrap();
                let t = t0.clone();

                output.push(t);
                self.substitute(rest, iter, args, ctx, output)
            }
        }
    }

    fn glue(
        &self,
        iter: &mut Option<&mut FragmentIterator>,
        mut left: Vec<(MacroToken, bool)>,
        mut right: Vec<(MacroToken, bool)>,
    ) -> Vec<(MacroToken, bool)> {
        //     if LS is ',' : []
        if left.len() == 1 && left[0].0.ty == MacroTokenType::Punctuation(Punctuation::Comma) {
            // and RS is empty, then
            //         return [];
            if right.is_empty() {
                right
            } else {
                // else if LS is ',' : [], then
                //     return LS : RS
                left.append(&mut right);
                left
            }
        }
        // if LS is L : [] and RS is R : RS', then
        else if left.len() == 1 && !right.is_empty() {
            // combine L and R as T
            // T.HS = intersection of L.HS and R.HS
            // return T:RS';
            let left_t = &left[0];
            let (right_t, rest) = right.split_first().unwrap();

            let combined = format!("{}{}", left_t.0, right_t.0);
            let mut tmp_iter = FragmentIterator::new(
                &iter.as_mut().map(|i| i.current_filename()).unwrap_or(""),
                &combined,
            );
            let mut tmp_ctx = MacroContext {
                symbols: HashMap::new(),
                iter: None,
                intern: StringInterner::new(),
            };

            let parsed_token = InternalMacroContext {
                get_file: unreachable_file_open,
                if_stack: Vec::new(),
                ctx: &mut tmp_ctx
            }
            .get_token(&mut tmp_iter, false);
            if tmp_iter.peek().is_some() {
                panic!("Could not properly parse a token from '{}'", combined);
            }
            assert!(parsed_token.0.is_some());
            if_debug(DebugVal::MacroExpand, || {
                println!(
                    "Combined {} and {} to {}",
                    left_t.0,
                    right_t.0,
                    parsed_token.0.as_ref().unwrap()
                )
            });
            let tok = MacroToken {
                source: Source::dummy(),
                ty: parsed_token.0.unwrap().ty,
            };

            let mut out = vec![(tok, left_t.1 && right_t.1)];
            out.append(&mut rest.to_vec());
            out
        } else if left.is_empty() {
            right
        } else {
            // else LS is L : LS', and
            //    return L : glue(LS', RS);
            let (t0, rest) = left.split_first().unwrap();
            let mut out = vec![t0.clone()];
            let mut glued = self.glue(iter, rest.to_vec(), right);
            out.append(&mut glued);
            out
        }
    }

    fn flush_pop_context<'a>(
        &self,
        mut list: &'a [(MacroToken, bool)],
        ctx: &mut ExpansionHistory,
    ) -> &'a [(MacroToken, bool)] {
        while let Some(MacroTokenType::PopContext) = list.get(0).as_ref().map(|t| &t.0.ty) {
            list = list.split_first().unwrap().1;
            ctx.pop_context();
        }
        list
    }

    #[allow(clippy::type_complexity)]
    fn parse_arguments<'a>(
        &mut self,
        mut input: &'a [(MacroToken, bool)],
        iter: &mut Option<&mut FragmentIterator>,
        args: &[Rc<str>],
        varargs: Option<&Rc<str>>,
    ) -> (
        &'a [(MacroToken, bool)],
        HashMap<Rc<str>, Vec<(MacroToken, bool)>>,
    ) {
        let has_varargs = varargs.is_some();

        let mut depth = 0;
        let mut argc = 0;
        let mut result = Vec::new();
        let mut row = Vec::new();
        let mut consumed = Vec::new();

        if input.is_empty() {
            let mut t = None;
            while iter.as_mut().and_then(|i| i.peek()).is_some() {
                if let Some(tt) = self.get_token(*iter.as_mut().unwrap(), false).0 {
                    t = Some(tt);
                    break;
                }
            }
            if let Some(t) = t {
                assert_eq!(t.ty, MacroTokenType::Punctuation(Punctuation::OpenParen));
            } else {
                panic!("Unexpected end of input");
            }
        } else {
            let (t, rest) = input.split_first().unwrap();
            input = rest;
            assert_eq!(t.0.ty, MacroTokenType::Punctuation(Punctuation::OpenParen));
        }

        loop {
            let next = if input.is_empty() {
                let mut t = None;
                while iter.as_mut().and_then(|i| i.peek()).is_some() {
                    if let Some(tt) = self.get_token(iter.as_mut().unwrap(), false).0 {
                        t = Some(tt);
                        break;
                    }
                }
                if let Some(t) = t {
                    consumed.push(t.clone());
                    (t, true)
                } else {
                    panic!("Unexpected end of input");
                }
            } else {
                let (t, rest) = input.split_first().unwrap();
                input = rest;
                t.clone()
            };

            match &next.0.ty {
                MacroTokenType::Punctuation(punc) => {
                    match punc {
                        Punctuation::OpenParen => {
                            depth += 1;
                        }
                        Punctuation::CloseParen => {
                            if depth == 0 {
                                break;
                            }
                            depth -= 1;
                        }
                        Punctuation::Comma => {
                            if depth == 0 {
                                if argc < args.len() {
                                    argc += 1;
                                    result.push(row);
                                    row = Vec::new();
                                    continue;
                                } else if !has_varargs {
                                    panic!("Unexpected argument");
                                }
                            }
                        }
                        _ => {}
                    }
                    row.push(next);
                }
                _ => row.push(next),
            }
        }

        result.push(row);
        argc += 1;

        if argc < args.len() {
            panic!("Not enough arguments");
        }

        let mut output = HashMap::new();

        for (i, arg) in args.iter().enumerate() {
            output.insert(arg.clone(), result[i].clone());
        }
        if let Some(s) = varargs {
            output.insert(
                s.clone(),
                result.get(args.len()).cloned().unwrap_or_else(Vec::new),
            );
        }

        (input, output)
    }

    fn next_is_open_paren(
        &mut self,
        input: &[(MacroToken, bool)],
        iter: &mut Option<&mut FragmentIterator>,
    ) -> bool {
        let mut index = 0;

        loop {
            match input.get(index).as_ref().map(|t| &t.0.ty) {
                Some(MacroTokenType::Punctuation(Punctuation::OpenParen)) => return true,
                Some(MacroTokenType::PopContext) => {
                    index += 1;
                }
                Some(_) => return false,
                None => {
                    if let Some(iter) = iter {
                        self.flush_whitespace(*iter);
                        match iter.peek() {
                            Some('(') => return true,
                            _ => return false,
                        }
                    } else {
                        return false;
                    }
                }
            }
        }
    }
}

fn unreachable_file_open(_: FileQuery) -> ResolveResult {
    unreachable!()
}

#[derive(Clone, Debug)]
struct ExpansionHistory {
    pub history: Vec<Rc<str>>,
}

impl ExpansionHistory {
    fn new() -> ExpansionHistory {
        ExpansionHistory {
            history: Vec::new(),
        }
    }

    fn contains(&self, other: &Rc<str>) -> bool {
        self.history.contains(other)
    }

    fn pop_context(&mut self) {
        self.history.pop().unwrap();
    }

    fn insert(&mut self, other: Rc<str>) {
        self.history.push(other);
    }
}

trait CommonMacros {
    fn empty(&mut self, ty: &str);
    fn num(&mut self, ty: &str, res: &str);
    fn ident(&mut self, ty: &str, res: &str);
}

impl CommonMacros for (StringInterner, HashMap<Rc<str>, Macro>) {
    fn empty(&mut self, ty: &str) {
        self.1.insert(
            self.0.get_ref(ty),
            Macro::Text(
                Source::dummy(),
                Vec::new()
            ),
        );
    }
    fn num(&mut self, ty: &str, res: &str) {
        self.1.insert(
            self.0.get_ref(ty),
            Macro::Text(
                Source::dummy(),
                vec![MacroToken {
                    ty: MacroTokenType::Number(self.0.get_ref(res)),
                    source: Source::dummy(),
                }],
            ),
        );
    }
    fn ident(&mut self, ty: &str, res: &str) {
        let syms = res.split(" ")
            .map(|ident|
                 MacroToken {
                     ty: MacroTokenType::Identifier(self.0.get_ref(ident)),
                     source: Source::dummy(),
                 })
            .collect();
        self.1.insert(self.0.get_ref(ty), Macro::Text(Source::dummy(), syms));
    }
}
