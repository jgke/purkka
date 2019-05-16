#![feature(box_patterns)]

use std::collections::HashSet;
use std::rc::Rc;

use cparser::grammar::*;
use ctoken::token::Token;

struct Context {
    indent: usize,
    newline: bool,
    whitespace: bool,
    buf: String,
}

pub fn format_c<H: std::hash::BuildHasher>(tree: &S, includes: HashSet<Rc<str>, H>) -> String {
    let mut buf = includes
        .into_iter()
        .map(|t| format!("#include \"{}\"\n", t))
        .collect::<Vec<_>>()
        .join("");
    if !buf.is_empty() {
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
            TranslationUnit::Units(units) => {
                for unit in units {
                    self.external_declaration(unit);
                }
            }
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
            Declaration::Declaration(decl, list) => {
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
            CompoundStatement::Statements(statements) => {
                self.push_token(&Token::OpenBrace(0));
                self.statement_list(statements);
                self.push_token(&Token::CloseBrace(0));
            }
        }
    }

    fn statement_list(&mut self, tree: &Vec<StatementOrDeclaration>) {
        for stat_or_decl in tree {
            match stat_or_decl {
                StatementOrDeclaration::Statement(statement) => self.statement(statement),
                StatementOrDeclaration::Declaration(decl) => self.declaration(decl),
            }
        }
    }

    fn statement(&mut self, tree: &Statement) {
        match tree {
            Statement::CompoundStatement(compound) => self.compound_statement(compound),
            Statement::ExpressionStatement(box ExpressionStatement::Expression(e)) => {
                e.as_ref().map(|e| self.expression(&**e));
                self.push_token(&Token::Semicolon(0));
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
            Expression::Expression(exprs) => {
                if let Some((last, rest)) = exprs.split_last() {
                    for decl in rest {
                        self.assignment_expression(decl);
                        self.push_token(&Token::Comma(0));
                    }
                    self.assignment_expression(last);
                }
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

    fn init_declarator_list(&mut self, tree: &Vec<InitDeclarator>) {
        if let Some((last, rest)) = tree.split_last() {
            for decl in rest {
                self.init_declarator(decl);
                self.push_token(&Token::Comma(0));
            }
            self.init_declarator(last);
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
            DirectDeclarator::Nothing => {}
            DirectDeclarator::ArrayOf(direct_decl, e) => {
                self.direct_declarator(&*direct_decl);
                self.push_token(&Token::OpenBracket(0));
                self.maybe_general_expression(&e.as_ref().map(|t| &**t));
                self.push_token(&Token::CloseBracket(0));
            }
            DirectDeclarator::Function(direct_decl, params) => {
                self.direct_declarator(&*direct_decl);
                self.push_token(&Token::OpenParen(0));
                self.parameter_type_list(params);
                self.push_token(&Token::OpenBracket(0));
            }
        }
    }

    fn maybe_general_expression(&mut self, tree: &Option<&GeneralExpression>) {
        match tree {
            None => {}
            f => panic!("Not implemented.: {:?}", f),
        }
    }

    fn parameter_type_list(&mut self, tree: &Vec<FunctionParam>) {
        if let Some((last, rest)) = tree.split_last() {
            for decl in rest {
                self.function_param(decl);
                self.push_token(&Token::Comma(0));
            }
            self.function_param(last);
        }
    }

    fn function_param(&mut self, tree: &FunctionParam) {
        panic!("Not implemented.: {:?}", tree);
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
            Declarator::Declarator(ptr, name, direct_decl) => {
                ptr.as_ref().map(|t| self.pointer(&**t));
                self.push_token(&Token::Identifier(0, name.clone()));
                self.direct_declarator(direct_decl);
            }
            Declarator::FunctionPointer(ptr, decl, direct_decl) => {
                ptr.as_ref().map(|t| self.pointer(&**t));
                self.declarator(decl);
                self.direct_declarator(direct_decl);
            }
        }
    }

    fn pointer(&mut self, tree: &Pointer) {
        match tree {
            Pointer::Ptr(qualifiers, ptr) => {
                self.whitespace = false;
                self.type_qualifiers(qualifiers);
                self.push_token(&Token::Times(0));
                ptr.as_ref().map(|t| self.pointer(&**t));
            }
        }
    }

    fn declaration_specifiers(&mut self, tree: &DeclarationSpecifiers) {
        match tree {
            DeclarationSpecifiers::DeclarationSpecifiers(spec, ty) => {
                spec.iter().for_each(|s| self.specifiers(s));
                ty.iter().for_each(|t| self.type_specifier(t));
            }
        }
    }

    fn specifiers(&mut self, tree: &Specifiers) {
        self.storage_specifiers(&tree.0);
        self.type_qualifiers(&tree.1);
    }

    fn sign_to_token<'a>(&self, sign: Option<bool>) -> Option<Token> {
        match sign {
            None => None,
            Some(true) => Some(Token::Signed(0)),
            Some(false) => Some(Token::Unsigned(0)),
        }
    }

    fn ty_to_token<'a>(&self, ty: PrimitiveType) -> Token {
        match ty {
            PrimitiveType::Char => Token::Char(0),
            PrimitiveType::Int => Token::Int(0),
            PrimitiveType::Long => Token::Long(0),
            PrimitiveType::LongLong => unimplemented!()
        }
    }

    fn type_specifier(&mut self, spec: &TypeSpecifier) {
        let (sign, token) = match spec {
            CType::Void => (None, Token::Void(0)),
            CType::Primitive(sign, ty) => (self.sign_to_token(*sign), self.ty_to_token(*ty)),
            CType::Custom(ident) => (None, Token::Identifier(0, ident.clone())),
            f => panic!("Not implemented: {:?}", f),
        };
        if let Some(token) = sign {
            self.push_token(&token);
        }
        self.push_token(&token);
    }

    fn storage_specifiers(&mut self, storage: &StorageClassSpecifiers) {
        if storage.extern_ { self.push_token(&Token::Extern(0)); }
        if storage.static_ { self.push_token(&Token::Static(0)); }
        if storage.inline { self.push_token(&Token::Inline(0)); }
        if storage.auto { self.push_token(&Token::Auto(0)); }
        if storage.register { self.push_token(&Token::Register(0)); }
    }

    fn type_qualifiers(&mut self, ty: &TypeQualifiers) {
        if ty.const_ { self.push_token(&Token::Const(0)); }
        if ty.volatile { self.push_token(&Token::Volatile(0)); }
    }
}
