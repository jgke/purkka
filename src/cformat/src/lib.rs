use cparser::parser::*;
use ctoken::token::Token;

struct Context {
    indent: usize,
    newline: bool,
    whitespace: bool,
    buf: String,
}

pub fn format_c(tree: &S) -> String {
    let mut context = Context {
        indent: 0,
        newline: false,
        whitespace: false,
        buf: "".to_string(),
    };
    context.s(tree);
    context.buf
}

impl Context {
    fn s(&mut self, tree: &S) {
        match tree {
            S::TranslationUnit(unit) => self.translation_unit(unit),
        }
        if self.newline {
            self.buf.push('\n');
        }
    }

    fn push(&mut self, s: &str) {
        if self.newline {
            self.newline = false;
            self.buf.push('\n');
            for _ in 1..=self.indent {
                self.buf.push_str("    ");
            }
        } else if self.whitespace {
            self.buf.push_str(" ");
        }
        self.buf.push_str(s);
    }

    fn push_token(&mut self, token: &Token) {
        let s = match token {
            // First set: Always space before and after
            Token::Increment(_) => "++",
            Token::Decrement(_) => "--",
            Token::BitAnd(_) => "&",
            Token::Times(_) => "*",
            Token::Plus(_) => "+",
            Token::Minus(_) => "-",
            Token::BitNot(_) => "~",
            Token::Not(_) => "!",
            Token::Divide(_) => "/",
            Token::Mod(_) => "%",
            Token::BitShiftLeft(_) => "<<",
            Token::BitShiftRight(_) => ">>",
            Token::LessThan(_) => "<",
            Token::MoreThan(_) => ">",
            Token::LessEqThan(_) => "<=",
            Token::MoreEqThan(_) => ">=",
            Token::Equals(_) => "==",
            Token::NotEquals(_) => "!=",
            Token::BitXor(_) => "^",
            Token::BitOr(_) => "|",
            Token::And(_) => "&&",
            Token::Or(_) => "||",
            Token::Ternary(_) => "?",
            Token::Assign(_) => "=",
            Token::TimesAssign(_) => "*=",
            Token::DivAssign(_) => "/=",
            Token::ModAssign(_) => "%=",
            Token::PlusAssign(_) => "+=",
            Token::MinusAssign(_) => "-=",
            Token::BitShiftLeftAssign(_) => "<<=",
            Token::BitShiftRightAssign(_) => ">>=",
            Token::BitAndAssign(_) => "&=",
            Token::BitXorAssign(_) => "^=",
            Token::BitOrAssign(_) => "|=",

            Token::Identifier(_, t) => t,

            // Special forms
            Token::Sizeof(_, _) => panic!(),
            Token::Asm(_, _) => panic!(),

            // Second set: Whitespace after, but not before
            t => {
                let s = match t {
                    // Keyword
                    Token::Auto(_) => "auto",
                    Token::Break(_) => "break",
                    Token::Case(_) => "case",
                    Token::Char(_) => "char",
                    Token::Const(_) => "const",
                    Token::Continue(_) => "continue",
                    Token::Default(_) => "default",
                    Token::Do(_) => "do",
                    Token::Double(_) => "double",
                    Token::Else(_) => "else",
                    Token::Enum(_) => "enum",
                    Token::Extern(_) => "extern",
                    Token::Float(_) => "float",
                    Token::For(_) => "for",
                    Token::Goto(_) => "goto",
                    Token::If(_) => "if",
                    Token::Int(_) => "int",
                    Token::Long(_) => "long",
                    Token::Register(_) => "register",
                    Token::Return(_) => "return",
                    Token::Short(_) => "short",
                    Token::Signed(_) => "signed",
                    Token::Static(_) => "static",
                    Token::Inline(_) => "inline",
                    Token::Struct(_) => "struct",
                    Token::Switch(_) => "switch",
                    Token::Typedef(_) => "typedef",
                    Token::Union(_) => "union",
                    Token::Unsigned(_) => "unsigned",
                    Token::Void(_) => "void",
                    Token::Volatile(_) => "volatile",
                    Token::While(_) => "while",

                    // Third set: No whitespace before or after
                    t => {
                        let s = match t {
                            Token::Macro(_) => "#",
                            Token::MacroPaste(_) => "##",
                            Token::Comma(_) => ",",
                            Token::Dot(_) => ".",
                            Token::Arrow(_) => "->",
                            Token::OpenBrace(_) => "{",
                            Token::CloseBrace(_) => "}",

                            // Punctuation
                            Token::OpenBracket(_) => "[",
                            Token::CloseBracket(_) => "]",
                            Token::OpenParen(_) => "(",
                            Token::CloseParen(_) => ")",
                            Token::Colon(_) => ":",
                            Token::Semicolon(_) => ";",
                            Token::Varargs(_) => "...",

                            t => panic!("Unhandled case: {:?}", t),
                        };
                        self.whitespace = false;
                        self.newline = false;
                        self.push(s);
                        return;
                    }
                };
                self.whitespace = false;
                self.push(s);
                self.whitespace = true;
                return;
            }
        };
        self.whitespace = true;
        self.push(s);
        self.whitespace = true;
    }

    fn translation_unit(&mut self, tree: &TranslationUnit) {
        match tree {
            TranslationUnit::ExternalDeclaration(decl) => self.external_declaration(decl),
            TranslationUnit::TranslationUnit(more, decl) => {
                self.translation_unit(more);
                self.external_declaration(decl);
            }
            TranslationUnit::Epsilon() => {}
        }
    }

    fn external_declaration(&mut self, tree: &ExternalDeclaration) {
        match tree {
            ExternalDeclaration::Declaration(decl) => self.declaration(decl),
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn declaration(&mut self, tree: &Declaration) {
        match tree {
            Declaration::DeclarationSpecifiers(decl, _) => self.declaration_specifiers(decl),
            Declaration::List(decl, list, _) => {
                self.declaration_specifiers(decl);
                self.init_declarator_list(list);
                self.whitespace = false;
                self.newline = false;
                self.push(";");
                self.newline = true;
            }
        }
    }

    fn init_declarator_list(&mut self, tree: &InitDeclaratorList) {
        match tree {
            InitDeclaratorList::InitDeclarator(decl) => {
                self.init_declarator(decl);
            }
            InitDeclaratorList::Comma(more, comma, decl) => {
                self.init_declarator_list(more);
                self.push_token(comma);
                self.init_declarator(decl);
            }
        }
    }

    fn init_declarator(&mut self, tree: &InitDeclarator) {
        match tree {
            InitDeclarator::Declarator(decl) => match decl {
                Declarator::Identifier(ident, direct_decl) => {
                    self.push_token(ident);
                    self.direct_declarator(direct_decl);
                }
                f => panic!("Not implemented.: {:?}", f),
            },
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn direct_declarator(&mut self, tree: &DirectDeclarator) {
        match tree {
            DirectDeclarator::Epsilon() => {}
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn declaration_specifiers(&mut self, tree: &DeclarationSpecifiers) {
        match tree {
            DeclarationSpecifiers::Neither(ty) => {
                self.type_specifier(ty);
            }
            DeclarationSpecifiers::Left(spec, ty) => {
                self.specifiers(spec);
                self.type_specifier(ty);
            }
            DeclarationSpecifiers::Right(ty, spec) => {
                self.type_specifier(ty);
                self.specifiers(spec);
            }
            DeclarationSpecifiers::Both(spec_1, ty, spec_2) => {
                self.specifiers(spec_1);
                self.type_specifier(ty);
                self.specifiers(spec_2);
            }
        }
    }

    fn specifiers(&mut self, tree: &Specifiers) {
        match tree {
            Specifiers::TypeQualifier(t) => self.push_token(self.token_from_qualifier(t)),
            Specifiers::StorageClassSpecifier(t) => self.push_token(self.token_from_storage(t)),
            Specifiers::TypeQualifierList(t, more) => {
                self.push_token(self.token_from_qualifier(t));
                self.specifiers(more);
            }
            Specifiers::StorageClassSpecifierList(t, more) => {
                self.push_token(self.token_from_storage(t));
                self.specifiers(more);
            }
        }
    }

    fn sign_to_token<'a>(&self, sign: &'a Sign) -> &'a Token {
        match sign {
            Sign::Signed(t) => t,
            Sign::Unsigned(t) => t,
        }
    }

    fn type_specifier(&mut self, spec: &TypeSpecifier) {
        let (sign, token) = match spec {
            TypeSpecifier::Void(t) => (None, t),
            TypeSpecifier::Char(t) => (None, t),
            TypeSpecifier::SignedChar(sign, t) => (Some(self.sign_to_token(sign)), t),
            TypeSpecifier::Short(t, MaybeInt::Epsilon()) => (None, t),
            TypeSpecifier::SignedShort(sign, t, MaybeInt::Epsilon()) => (Some(self.sign_to_token(sign)), t),
            TypeSpecifier::Int(t) => (None, t),
            TypeSpecifier::SignedInt(sign, t) => (Some(self.sign_to_token(sign)), t),
            TypeSpecifier::Signed(sign) => (None, self.sign_to_token(sign)),
            f => panic!("Not implemented: {:?}", f),
        };
        sign.map(|token| self.push_token(token));
        self.push_token(token);
    }

    fn token_from_storage<'a>(&self, spec: &'a StorageClassSpecifier) -> &'a Token {
        match spec {
            StorageClassSpecifier::Extern(t) => t,
            StorageClassSpecifier::Static(t) => t,
            StorageClassSpecifier::Inline(t) => t,
            StorageClassSpecifier::Auto(t) => t,
            StorageClassSpecifier::Register(t) => t,
        }
    }

    fn token_from_qualifier<'a>(&self, spec: &'a TypeQualifier) -> &'a Token {
        match spec {
            TypeQualifier::Const(t) => t,
            TypeQualifier::Volatile(t) => t,
        }
    }
}
