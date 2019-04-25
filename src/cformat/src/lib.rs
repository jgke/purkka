#![feature(box_patterns)]

use std::collections::HashSet;
use std::rc::Rc;

use cparser::parser::*;
use ctoken::token::Token;

struct Context {
    indent: usize,
    newline: bool,
    whitespace: bool,
    buf: String,
}

pub fn format_c(tree: &S, includes: HashSet<Rc<str>>) -> String {
    let mut buf = includes
        .into_iter()
        .map(|t| format!("#include \"{}\"\n", t))
        .collect::<Vec<_>>()
        .join("");
    if buf.len() > 0 {
        buf += "\n";
    }
    let mut context = Context {
        indent: 0,
        newline: false,
        whitespace: false,
        buf,
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

            // Special forms
            Token::Sizeof(_, _) => panic!(),
            Token::Asm(_, _) => panic!(),

            // Second set: Whitespace after
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
                    Token::Identifier(_, t) => t,

                    // Third set: No whitespace before or after
                    t => {
                        let s = match t {
                            Token::Macro(_) => "#",
                            Token::MacroPaste(_) => "##",
                            Token::Dot(_) => ".",
                            Token::Arrow(_) => "->",

                            // Punctuation
                            Token::OpenBracket(_) => "[",
                            Token::CloseBracket(_) => "]",
                            Token::OpenParen(_) => "(",
                            Token::CloseParen(_) => ")",
                            Token::Colon(_) => ":",
                            Token::Semicolon(_) => ";",
                            Token::Varargs(_) => "...",

                            // Special cases
                            t => match t {
                                Token::OpenBrace(_) => {
                                    self.newline = false;
                                    self.whitespace = true;
                                    self.push("{");
                                    self.newline = true;
                                    self.indent += 1;
                                    return;
                                }
                                Token::CloseBrace(_) => {
                                    self.newline = true;
                                    self.indent -= 1;
                                    self.push("}");
                                    self.newline = true;
                                    return;
                                }
                                Token::Comma(_) => {
                                    self.newline = false;
                                    self.whitespace = false;
                                    self.push(",");
                                    self.whitespace = true;
                                    return;
                                }
                                Token::Times(_) => {
                                    self.push("*");
                                    return;
                                }
                                t => panic!("Unhandled case: {:?}", t),
                            },
                        };
                        self.whitespace = false;
                        self.newline = false;
                        self.push(s);
                        return;
                    }
                };
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
            ExternalDeclaration::FunctionDefinition(def) => self.function_definition(def),
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

    fn function_definition(&mut self, tree: &FunctionDefinition) {
        match tree {
            FunctionDefinition::Specifiers(decl_spec, decl, compound) => {
                self.declaration_specifiers(decl_spec);
                self.declarator(decl);
                self.compound_statement(compound);
            }
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn compound_statement(&mut self, tree: &CompoundStatement) {
        match tree {
            CompoundStatement::PushScope(PushScope::OpenBrace(open), statements, close) => {
                self.push_token(open);
                self.statement_list(&*statements);
                self.push_token(close);
            }
        }
    }

    fn statement_list(&mut self, tree: &StatementList) {
        match tree {
            StatementList::Epsilon() => {}
            StatementList::StatementOrDeclaration(StatementOrDeclaration::Statement(statement)) => {
                self.statement(statement)
            }
            StatementList::StatementOrDeclaration(StatementOrDeclaration::Declaration(decl)) => {
                self.declaration(decl)
            }
            StatementList::More(list, StatementOrDeclaration::Statement(statement)) => {
                self.statement_list(list);
                self.statement(statement);
            }
            StatementList::More(list, StatementOrDeclaration::Declaration(decl)) => {
                self.statement_list(list);
                self.declaration(decl);
            }
        }
    }

    fn statement(&mut self, tree: &Statement) {
        match tree {
            Statement::CompoundStatement(compound) => self.compound_statement(compound),
            Statement::ExpressionStatement(box ExpressionStatement::Semicolon(t)) => {
                self.push_token(t)
            }
            Statement::ExpressionStatement(box ExpressionStatement::Expression(e, t)) => {
                self.expression(e);
                self.push_token(t);
            }
            Statement::JumpStatement(box JumpStatement::ReturnVoid(r, t)) => {
                self.push_token(r);
                self.push_token(t);
            }
            Statement::JumpStatement(box JumpStatement::Return(r, e, t)) => {
                self.push_token(r);
                self.expression(e);
                self.push_token(t);
            }
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn expression(&mut self, tree: &Expression) {
        match tree {
            Expression::AssignmentExpression(e) => self.assignment_expression(e),
            Expression::Comma(list, c, e) => {
                self.expression(list);
                self.push_token(c);
                self.assignment_expression(e);
            }
        }
    }

    fn assignment_expression(&mut self, tree: &AssignmentExpression) {
        match tree {
            AssignmentExpression::TernaryExpression(e) => self.ternary_expression(e),
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn ternary_expression(&mut self, tree: &TernaryExpression) {
        match tree {
            TernaryExpression::GeneralExpression(e) => self.general_expression(e),
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn general_expression(&mut self, tree: &GeneralExpression) {
        match tree {
            GeneralExpression::CastExpression(e) => self.cast_expression(e),
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn cast_expression(&mut self, tree: &CastExpression) {
        match tree {
            CastExpression::UnaryExpression(e) => self.unary_expression(e),
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn unary_expression(&mut self, tree: &UnaryExpression) {
        match tree {
            UnaryExpression::PostfixExpression(e) => self.postfix_expression(e),
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn postfix_expression(&mut self, tree: &PostfixExpression) {
        match tree {
            PostfixExpression::PrimaryExpression(e) => self.primary_expression(e),
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn primary_expression(&mut self, tree: &PrimaryExpression) {
        match tree {
            PrimaryExpression::Identifier(Token::Identifier(_, e)) => self.push(e),
            PrimaryExpression::Number(Token::Number(_, e)) => self.push(e),
            PrimaryExpression::Asm(Token::Asm(_, _e)) => { /* XXX: implement */ }
            PrimaryExpression::Expression(op, expr, cp) => {
                self.push_token(op);
                self.expression(expr);
                self.push_token(cp);
            }
            PrimaryExpression::Statement(op, stat, cp) => {
                self.push_token(op);
                self.compound_statement(stat);
                self.push_token(cp);
            }
            f => panic!("Not implemented.: {:?}", f),
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
            InitDeclarator::Declarator(decl) => self.declarator(decl),
            InitDeclarator::Assign(decl, t, list) => {
                self.declarator(decl);
                self.push_token(t);
                self.assignment_or_initializer_list(list);
            }
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn assignment_or_initializer_list(&mut self, tree: &AssignmentOrInitializerList) {
        match tree {
            AssignmentOrInitializerList::AssignmentExpression(e) => self.assignment_expression(e),
            AssignmentOrInitializerList::OpenBrace(ob, list, cb) => {
                self.push_token(ob);
                self.initializer_list(list);
                self.push_token(cb);
            }
        }
    }

    fn initializer_list(&mut self, tree: &InitializerList) {
        match tree {
            InitializerList::Epsilon() => {}
            InitializerList::InitializerListContent(list, tc) => {
                self.initializer_list_content(list);
                self.trailing_comma(tc);
            }
        }
    }

    fn initializer_list_content(&mut self, tree: &InitializerListContent) {
        match tree {
            InitializerListContent::Initializer(init) => self.initializer(init),
            InitializerListContent::InitializerListContent(list, comma, init) => {
                self.initializer_list_content(list);
                self.push_token(comma);
                self.initializer(init);
            }
        }
    }

    fn initializer(&mut self, tree: &Initializer) {
        match tree {
            Initializer::AssignmentOrInitializerList(list) => {
                self.assignment_or_initializer_list(list)
            }
            Initializer::Dot(dot, ident, assign, list) => {
                self.push_token(dot);
                self.push_token(ident);
                self.push_token(assign);
                self.assignment_or_initializer_list(list);
            }
        }
    }

    fn trailing_comma(&mut self, tree: &TrailingComma) {
        match tree {
            TrailingComma::Epsilon() => {}
            TrailingComma::Comma(t) => self.push_token(t),
        }
    }

    fn direct_declarator(&mut self, tree: &DirectDeclarator) {
        match tree {
            DirectDeclarator::Epsilon() => {}
            DirectDeclarator::Array(ty_of, op, e, cb) => {
                self.direct_declarator(&*ty_of);
                self.push_token(op);
                self.maybe_general_expression(e);
                self.push_token(cb);
            }
            DirectDeclarator::FunctionParams(ty_of, op, fp, cb) => {
                self.direct_declarator(&*ty_of);
                self.push_token(op);
                self.function_params(fp);
                self.push_token(cb);
            }
            DirectDeclarator::Function(ty_of, op, cb) => {
                self.direct_declarator(&*ty_of);
                self.push_token(op);
                self.push_token(cb);
            }
        }
    }

    fn maybe_general_expression(&mut self, tree: &MaybeGeneralExpression) {
        match tree {
            MaybeGeneralExpression::Epsilon() => {}
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn function_params(&mut self, tree: &FunctionParams) {
        match tree {
            FunctionParams::ParameterTypeList(params) => {
                self.parameter_type_list(params);
            }
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn parameter_type_list(&mut self, tree: &ParameterTypeList) {
        match tree {
            ParameterTypeList::ParameterList(params) => {
                self.parameter_list(params);
            }
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn parameter_list(&mut self, tree: &ParameterList) {
        match tree {
            ParameterList::ParameterDeclaration(param) => {
                self.parameter_declaration(param);
            }
            ParameterList::ParameterList(params, t, param) => {
                self.parameter_list(params);
                self.push_token(t);
                self.parameter_declaration(param);
            }
        }
    }

    fn parameter_declaration(&mut self, tree: &ParameterDeclaration) {
        match tree {
            ParameterDeclaration::Declarator(decl_spec, decl) => {
                self.declaration_specifiers(decl_spec);
                self.declarator(decl);
            }
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn declarator(&mut self, tree: &Declarator) {
        match tree {
            Declarator::Identifier(ident, direct_decl) => {
                self.push_token(ident);
                self.direct_declarator(direct_decl);
            }
            Declarator::Pointer(p, IdentifierOrType::Identifier(ident), direct_decl) => {
                self.pointer(p);
                self.push_token(ident);
                self.direct_declarator(direct_decl);
            }
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn pointer(&mut self, tree: &Pointer) {
        match tree {
            Pointer::Times(t) => {
                self.whitespace = false;
                self.push_token(t);
            }
            Pointer::Pointer(t, p) => {
                self.push_token(t);
                self.pointer(p);
            }
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
            TypeSpecifier::SignedShort(sign, t, MaybeInt::Epsilon()) => {
                (Some(self.sign_to_token(sign)), t)
            }
            TypeSpecifier::Int(t) => (None, t),
            TypeSpecifier::SignedInt(sign, t) => (Some(self.sign_to_token(sign)), t),
            TypeSpecifier::Signed(sign) => (None, self.sign_to_token(sign)),
            TypeSpecifier::TypeNameStr(t) => (None, t),
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
