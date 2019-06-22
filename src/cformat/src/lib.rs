#![feature(box_patterns)]

use std::collections::HashSet;
use std::rc::Rc;

use cparser::grammar::*;
use ctoken::token::Token;

#[derive(Debug)]
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

macro_rules! op_3 {
    ($this:ident, $left:ident, $op:ident, $right:ident) => {{
        $this.general_expression($left);
        $this.push_token($op);
        $this.general_expression($right);
    }};
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
                    Token::Number(_, t) => t,
                    Token::Sizeof(_) => "sizeof",
                    Token::Asm(_) => "asm",

                    // Third set: No whitespace before or after
                    t => {
                        let s = match t {
                            Token::Macro(_) => "#",
                            Token::MacroPaste(_) => "##",
                            Token::Dot(_) => ".",
                            Token::Arrow(_) => "->",
                            Token::Increment(_) => "++",
                            Token::Decrement(_) => "--",

                            // Punctuation
                            Token::OpenBracket(_) => "[",
                            Token::CloseBracket(_) => "]",
                            Token::OpenParen(_) => "(",
                            Token::CloseParen(_) => ")",
                            Token::Colon(_) => ":",
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
                                Token::StringLiteral(_, s) => {
                                    self.push("*");
                                    self.push("\"");
                                    self.whitespace = false;
                                    self.push(s);
                                    self.push("\"");
                                    return;
                                }
                                Token::CharLiteral(_, s) => {
                                    self.push("'");
                                    self.whitespace = false;
                                    self.push(&s.to_string());
                                    self.push("'");
                                    return;
                                }
                                Token::Semicolon(_) => {
                                    self.whitespace = false;
                                    self.push(";");
                                    self.newline = true;
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
            FunctionDefinition::FunctionDefinition(decl_spec, decls, compound) => {
                decl_spec
                    .iter()
                    .for_each(|d| self.declaration_specifiers(d));
                for decl in decls {
                    self.declarator(decl);
                }
                self.compound_statement(compound);
            }
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

    fn statement_list(&mut self, tree: &[StatementOrDeclaration]) {
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
            Statement::ExpressionStatement(expr) => self.expression_statement(&**expr),
            Statement::JumpStatement(jumpy) => self.jump_statement(jumpy),
            Statement::SelectionStatement(statement) => self.selection_statement(statement),
            Statement::LabeledStatement(statement) => self.labeled_statement(statement),
            Statement::IterationStatement(statement) => self.iteration_statement(statement),
            Statement::AsmStatement(asm) => {
                self.asm_statement(asm);
                self.push_token(&Token::Semicolon(0));
            }
            Statement::TypeDeclaration(..) => unimplemented!(),
        }
    }

    fn expression_statement(&mut self, tree: &ExpressionStatement) {
        match tree {
            ExpressionStatement::Expression(e) => {
                if let Some(e) = e.as_ref() {
                    self.expression(&**e)
                }
                self.push_token(&Token::Semicolon(0));
            }
        }
    }

    fn selection_statement(&mut self, tree: &SelectionStatement) {
        match tree {
            SelectionStatement::If(e, s, otherwise) => {
                self.push_token(&Token::If(0));
                self.push_token(&Token::OpenParen(0));
                self.expression(&**e);
                self.push_token(&Token::CloseParen(0));
                self.whitespace = true;
                self.statement(s);
                if let Some(e) = otherwise {
                    self.push_token(&Token::Else(0));
                    self.statement(e);
                }
            }
            SelectionStatement::Switch(e, s) => {
                self.push_token(&Token::Switch(0));
                self.push_token(&Token::OpenParen(0));
                self.expression(&**e);
                self.push_token(&Token::CloseParen(0));
                self.whitespace = true;
                self.statement(s);
            }
        }
    }

    fn labeled_statement(&mut self, tree: &LabeledStatement) {
        match tree {
            LabeledStatement::Case(case, e, c, s) => {
                self.push_token(case);
                self.general_expression(&**e);
                self.push_token(c);
                self.statement(s);
            }
            LabeledStatement::RangeCase(case, e, v, e2, c, s) => {
                self.push_token(case);
                self.general_expression(&**e);
                self.push_token(v);
                self.general_expression(&**e2);
                self.push_token(c);
                self.statement(s);
            }
            LabeledStatement::Default(kw, c, s) => {
                self.push_token(kw);
                self.push_token(c);
                self.statement(s);
            }
            LabeledStatement::Identifier(ident, c, s) => {
                self.push_token(ident);
                self.push_token(c);
                self.statement(s);
            }
        }
    }

    fn iteration_statement(&mut self, tree: &IterationStatement) {
        match tree {
            IterationStatement::While(w, op, e, cp, s) => {
                self.push_token(w);
                self.push_token(op);
                self.expression(&**e);
                self.push_token(cp);
                self.statement(s);
            }
            IterationStatement::Do(d, s, w, op, e, cp, semi) => {
                self.push_token(d);
                self.statement(s);
                self.push_token(w);
                self.push_token(op);
                self.expression(&**e);
                self.push_token(cp);
                self.push_token(semi);
            }
            IterationStatement::For(f, op, ForExpr::EmptyLast(e1, e2), cp, s) => {
                self.push_token(f);
                self.push_token(op);
                match e1 {
                    DeclarationOrExpression::Declaration(decl) => self.declaration(decl),
                    DeclarationOrExpression::ExpressionStatement(e) => self.expression_statement(e),
                }
                self.newline = false;
                self.whitespace = true;
                self.expression_statement(&**e2);
                self.newline = false;
                self.push_token(cp);
                self.statement(s);
            }
            IterationStatement::For(f, op, ForExpr::ForExpr(e1, e2, e3), cp, s) => {
                self.push_token(f);
                self.push_token(op);
                match e1 {
                    DeclarationOrExpression::Declaration(decl) => self.declaration(decl),
                    DeclarationOrExpression::ExpressionStatement(e) => self.expression_statement(e),
                }
                self.newline = false;
                self.whitespace = true;
                self.expression_statement(&**e2);
                self.newline = false;
                self.whitespace = true;
                self.expression(&**e3);
                self.push_token(cp);
                self.statement(s);
            }
        }
    }

    fn asm_statement(&mut self, tree: &AsmStatement) {
        self.push_token(&Token::Asm(0));
        let AsmStatement::Asm {
            tokens,
            volatile,
            goto,
            inline,
        } = tree;
        if *volatile {
            self.push_token(&Token::Volatile(0));
        }
        if *goto {
            self.push_token(&Token::Goto(0));
        }
        if *inline {
            self.push_token(&Token::Inline(0));
        }
        for t in tokens {
            self.push_token(t);
        }
    }

    fn jump_statement(&mut self, tree: &JumpStatement) {
        match tree {
            JumpStatement::Goto(kw, ident, s) => {
                self.push_token(kw);
                self.push_token(ident);
                self.push_token(s);
            }
            JumpStatement::Continue(kw, s) => {
                self.push_token(kw);
                self.push_token(s);
            }
            JumpStatement::Break(kw, s) => {
                self.push_token(kw);
                self.push_token(s);
            }
            JumpStatement::ReturnVoid(r, t) => {
                self.push_token(r);
                self.push_token(t);
            }
            JumpStatement::Return(r, e, t) => {
                self.push_token(r);
                self.expression(e);
                self.push_token(t);
            }
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
            AssignmentExpression::Assignment(unary, assignment, expr) => {
                self.unary_expression(unary);
                match assignment {
                    AssignmentOperator::Assign(op) => self.push_token(op),
                    AssignmentOperator::TimesAssign(op) => self.push_token(op),
                    AssignmentOperator::DivAssign(op) => self.push_token(op),
                    AssignmentOperator::ModAssign(op) => self.push_token(op),
                    AssignmentOperator::PlusAssign(op) => self.push_token(op),
                    AssignmentOperator::MinusAssign(op) => self.push_token(op),
                    AssignmentOperator::BitShiftLeftAssign(op) => self.push_token(op),
                    AssignmentOperator::BitShiftRightAssign(op) => self.push_token(op),
                    AssignmentOperator::BitAndAssign(op) => self.push_token(op),
                    AssignmentOperator::BitXorAssign(op) => self.push_token(op),
                    AssignmentOperator::BitOrAssign(op) => self.push_token(op),
                }
                self.assignment_expression(&**expr);
            }
        }
    }

    fn ternary_expression(&mut self, tree: &TernaryExpression) {
        match tree {
            TernaryExpression::GeneralExpression(e) => self.general_expression(e),
            TernaryExpression::Ternary(ge, q, e, c, t) => {
                self.general_expression(ge);
                self.push_token(q);
                self.expression(&**e);
                self.push_token(c);
                self.ternary_expression(&**t);
            }
        }
    }

    fn general_expression(&mut self, tree: &GeneralExpression) {
        match tree {
            GeneralExpression::CastExpression(e) => self.cast_expression(e),

            GeneralExpression::Times(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::Divide(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::Mod(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::Plus(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::Minus(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::BitShiftLeft(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::BitShiftRight(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::LessThan(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::MoreThan(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::LessEqThan(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::MoreEqThan(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::Equals(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::NotEquals(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::BitAnd(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::BitXor(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::BitOr(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::And(left, op, right) => op_3!(self, left, op, right),
            GeneralExpression::Or(left, op, right) => op_3!(self, left, op, right),
        }
    }

    fn cast_expression(&mut self, tree: &CastExpression) {
        match tree {
            CastExpression::UnaryExpression(e) => self.unary_expression(e),
            CastExpression::OpenParen(op, ty, cp, e) => {
                self.push_token(op);
                self.type_name(&**ty);
                self.push_token(cp);
                self.general_expression(&**e);
            }
        }
    }

    fn type_name(&mut self, tree: &TypeName) {
        let TypeName::TypeName(spec, abs) = tree;
        self.declaration_specifiers(spec);
        self.abstract_declarator(&**abs);
    }

    fn abstract_declarator(&mut self, tree: &AbstractDeclarator) {
        match tree {
            AbstractDeclarator::AbstractDeclarator(ptr, abs) => {
                if let Some(p) = ptr {
                    self.pointer(p);
                }
                self.direct_abstract_declarator(&**abs)
            }
        }
    }

    fn direct_abstract_declarator(&mut self, tree: &DirectAbstractDeclarator) {
        match tree {
            DirectAbstractDeclarator::Epsilon() => {}
            DirectAbstractDeclarator::Parens(d) => {
                self.push_token(&Token::OpenParen(0));
                self.abstract_declarator(&**d);
                self.push_token(&Token::CloseParen(0));
            }
            DirectAbstractDeclarator::Array(d, e) => {
                self.direct_abstract_declarator(&**d);
                self.push_token(&Token::OpenBracket(0));
                if let Some(e) = e {
                    self.general_expression(&**e);
                }
                self.push_token(&Token::CloseBracket(0));
            }
            DirectAbstractDeclarator::Function(d, params) => {
                self.direct_abstract_declarator(&**d);
                self.push_token(&Token::OpenParen(0));
                self.parameter_type_list(params);
                self.push_token(&Token::CloseParen(0));
            }
        }
    }

    fn unary_expression(&mut self, tree: &UnaryExpression) {
        match tree {
            UnaryExpression::PostfixExpression(e) => self.postfix_expression(e),
            UnaryExpression::IncrementOrDecrement(inc, e) => {
                match inc {
                    IncrementOrDecrement::Increment(op) => self.push_token(op),
                    IncrementOrDecrement::Decrement(op) => self.push_token(op),
                }
                self.unary_expression(&**e);
            }
            UnaryExpression::UnaryOperator(un_op, e) => {
                match un_op {
                    UnaryOperator::BitAnd(t) => self.push_token(t),
                    UnaryOperator::Times(t) => self.push_token(t),
                    UnaryOperator::Plus(t) => self.push_token(t),
                    UnaryOperator::Minus(t) => self.push_token(t),
                    UnaryOperator::BitNot(t) => self.push_token(t),
                    UnaryOperator::Not(t) => self.push_token(t),
                }
                self.cast_expression(&**e);
            }
            UnaryExpression::AddressOfLabel(label) => {
                self.push_token(&Token::And(0));
                self.push_token(&Token::Identifier(0, label.clone()));
            }
            UnaryExpression::SizeofExpr(e) => {
                self.push("sizeof");
                self.unary_expression(&**e);
            }
            UnaryExpression::SizeofTy(e) => {
                self.push("sizeof");
                self.push_token(&Token::OpenParen(0));
                self.type_name(&**e);
                self.push_token(&Token::CloseParen(0));
            }
        }
    }

    fn postfix_expression(&mut self, tree: &PostfixExpression) {
        match tree {
            PostfixExpression::PrimaryExpression(e) => self.primary_expression(e),
            PostfixExpression::Call(pe, op, list, cp) => {
                self.postfix_expression(pe);
                self.push_token(op);
                match list {
                    ArgumentExpressionList::List(list) => {
                        if let Some((last, rest)) = list.split_last() {
                            for expr in rest {
                                self.assignment_expression(expr);
                                self.push_token(&Token::Comma(0));
                            }
                            self.assignment_expression(last);
                        }
                    }
                }
                self.push_token(cp);
            }
            PostfixExpression::Increment(pe, inc) => {
                self.postfix_expression(pe);
                match inc {
                    IncrementOrDecrement::Increment(op) => self.push_token(op),
                    IncrementOrDecrement::Decrement(op) => self.push_token(op),
                }
            }
            PostfixExpression::Member(pe, access, ident) => {
                self.postfix_expression(pe);
                match access {
                    MemberAccess::Dot(op) => self.push_token(op),
                    MemberAccess::Arrow(op) => self.push_token(op),
                }
                self.push_token(ident);
            }
            PostfixExpression::Index(pe, ob, e, cb) => {
                self.postfix_expression(pe);
                self.push_token(ob);
                self.expression(e);
                self.push_token(cb);
            }
        }
    }

    fn primary_expression(&mut self, tree: &PrimaryExpression) {
        match tree {
            PrimaryExpression::Identifier(val) => self.push(val),
            PrimaryExpression::Number(val) => self.push(val),
            PrimaryExpression::Expression(expr) => {
                self.push_token(&Token::OpenParen(0));
                self.expression(expr);
                self.push_token(&Token::CloseParen(0));
            }
            PrimaryExpression::Statement(stat) => {
                self.push_token(&Token::OpenParen(0));
                self.compound_statement(stat);
                self.push_token(&Token::CloseParen(0));
            }
            PrimaryExpression::StructValue(ty, list) => {
                self.push_token(&Token::OpenParen(0));
                self.type_name(&**ty);
                self.push_token(&Token::CloseParen(0));
                self.push_token(&Token::OpenBrace(0));
                if let Some((last, rest)) = list.split_last() {
                    for init in rest {
                        self.initializer(init);
                        self.push_token(&Token::Comma(0));
                    }
                    self.initializer(last);
                }
                self.push_token(&Token::CloseBrace(0));
            }
            PrimaryExpression::Sizeof(toks) => {
                self.push("sizeof");
                self.push_token(&Token::OpenParen(0));
                for t in toks.iter() {
                    self.push_token(t);
                }
                self.push_token(&Token::CloseParen(0));
            }
            PrimaryExpression::StringLiteral(s) => {
                self.push("\"");
                self.whitespace = false;
                self.push(s);
                self.push("\"");
            }
            PrimaryExpression::CharLiteral(s) => {
                self.push("'");
                self.whitespace = false;
                self.push(&s.to_string());
                self.push("'");
            }
            PrimaryExpression::Builtin(extension) => {
                self.builtin(&**extension);
            }
        }
    }

    fn builtin(&mut self, builtin: &Builtin) {
        match builtin {
            Builtin::Offsetof(ty, designator) => {
                self.push_token(&Token::Identifier(0, From::from("__builtin_offsetof")));
                self.push_token(&Token::OpenParen(0));
                self.type_name(&**ty);
                self.push_token(&Token::Comma(0));
                self.builtin_designator(designator);
                self.push_token(&Token::CloseParen(0));
            }
            Builtin::TypesCompatible(left, right) => {
                self.push_token(&Token::Identifier(
                    0,
                    From::from("__builtin_types_compatible_p"),
                ));
                self.push_token(&Token::OpenParen(0));
                self.type_name(&**left);
                self.push_token(&Token::Comma(0));
                self.type_name(&**right);
                self.push_token(&Token::CloseParen(0));
            }
        }
    }

    fn builtin_designator(&mut self, builtin: &BuiltinDesignator) {
        match builtin {
            BuiltinDesignator::Identifier(i) => self.push(i),
            BuiltinDesignator::Field(d, i) => {
                self.builtin_designator(d);
                self.push_token(&Token::Comma(0));
                self.push(i);
            }
            BuiltinDesignator::Index(d, expr) => {
                self.builtin_designator(d);
                self.push_token(&Token::OpenBracket(0));
                self.expression(&**expr);
                self.push_token(&Token::CloseBracket(0));
            }
        }
    }

    fn init_declarator_list(&mut self, tree: &[InitDeclarator]) {
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
            InitDeclarator::Asm(decl, asm) => {
                self.declarator(decl);
                self.asm_statement(asm);
            }
            InitDeclarator::Assign(decl, t, list) => {
                self.declarator(decl);
                self.push_token(t);
                self.assignment_or_initializer_list(list);
            }
        }
    }

    fn assignment_or_initializer_list(&mut self, tree: &AssignmentOrInitializerList) {
        match tree {
            AssignmentOrInitializerList::AssignmentExpression(e) => self.assignment_expression(e),
            AssignmentOrInitializerList::Initializers(list) => {
                self.push_token(&Token::OpenBrace(0));
                self.initializer_list(list);
                self.push_token(&Token::CloseBrace(0));
            }
        }
    }

    fn initializer_list(&mut self, tree: &[Initializer]) {
        for initializer in tree {
            self.initializer(initializer);
            self.push_token(&Token::Comma(0));
        }
    }

    fn initializer(&mut self, tree: &Initializer) {
        match tree {
            Initializer::Initializer(name, list) => {
                if let Some(ident) = name {
                    self.push_token(&Token::Dot(0));
                    self.push_token(&Token::Identifier(0, ident.clone()));
                    self.push_token(&Token::Assign(0));
                }
                self.assignment_or_initializer_list(list);
            }
        }
    }

    fn direct_declarator(&mut self, tree: &DirectDeclarator) {
        match tree {
            DirectDeclarator::Identifier(ident) => {
                self.push_token(&Token::Identifier(0, ident.clone()))
            }
            DirectDeclarator::Parens(decl) => {
                self.push_token(&Token::OpenParen(0));
                self.declarator(decl);
                self.push_token(&Token::CloseParen(0));
            }
            DirectDeclarator::Array(direct_decl, e) => {
                self.direct_declarator(&*direct_decl);
                self.push_token(&Token::OpenBracket(0));
                self.maybe_general_expression(e.as_ref().map(|t| &**t));
                self.push_token(&Token::CloseBracket(0));
            }
            DirectDeclarator::Function(direct_decl, params) => {
                self.whitespace = false;
                self.push(" ");
                self.direct_declarator(&*direct_decl);
                self.push_token(&Token::OpenParen(0));
                self.parameter_type_list(params);
                self.push_token(&Token::CloseParen(0));
            }
        }
    }

    fn maybe_general_expression(&mut self, tree: Option<&GeneralExpression>) {
        if let Some(t) = tree {
            self.general_expression(t);
        }
    }

    fn parameter_type_list(&mut self, tree: &[FunctionParam]) {
        if let Some((last, rest)) = tree.split_last() {
            for decl in rest {
                self.function_param(decl);
                self.push_token(&Token::Comma(0));
            }
            self.function_param(last);
        }
    }

    fn function_param(&mut self, tree: &FunctionParam) {
        match tree {
            FunctionParam::Identifier(ident) => {
                self.push_token(&Token::Identifier(0, ident.clone()))
            }
            FunctionParam::Parameter(param) => self.parameter_declaration(param),
            FunctionParam::Varargs => self.push_token(&Token::Varargs(0)),
        }
    }
    fn parameter_declaration(&mut self, tree: &ParameterDeclaration) {
        match tree {
            ParameterDeclaration::Declarator(decl_spec, decl) => {
                self.declaration_specifiers(decl_spec);
                self.declarator(decl);
            }
            ParameterDeclaration::DeclarationSpecifiers(decl_spec) => {
                self.declaration_specifiers(decl_spec);
            }
            ParameterDeclaration::AbstractDeclarator(decl_spec, abst) => {
                self.declaration_specifiers(decl_spec);
                self.abstract_declarator(abst);
            }
        }
    }

    fn declarator(&mut self, tree: &Declarator) {
        match tree {
            Declarator::Declarator(ptr, direct_decl) => {
                if let Some(t) = ptr.as_ref() {
                    self.pointer(&**t)
                }
                self.direct_declarator(direct_decl);
            }
        }
    }

    fn pointer(&mut self, tree: &Pointer) {
        match tree {
            Pointer::Ptr(qualifiers, ptr) => {
                self.push_token(&Token::Times(0));
                self.type_qualifiers(qualifiers);
                if !qualifiers.any() {
                    self.whitespace = false;
                }
                if let Some(t) = ptr.as_ref() {
                    self.pointer(&**t)
                }
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

    fn sign_to_token(&self, sign: Option<bool>) -> Option<Token> {
        match sign {
            None => None,
            Some(true) => Some(Token::Signed(0)),
            Some(false) => Some(Token::Unsigned(0)),
        }
    }

    fn ty_to_token(&self, ty: PrimitiveType) -> (bool, Token) {
        match ty {
            PrimitiveType::Char => (false, Token::Char(0)),
            PrimitiveType::Short => (false, Token::Short(0)),
            PrimitiveType::Int => (false, Token::Int(0)),
            PrimitiveType::Long => (false, Token::Long(0)),
            PrimitiveType::Float => (false, Token::Float(0)),
            PrimitiveType::Double => (false, Token::Double(0)),
            PrimitiveType::LongLong => (true, Token::Long(0)),
            PrimitiveType::LongDouble => (true, Token::Double(0)),
        }
    }

    fn type_specifier(&mut self, spec: &TypeSpecifier) {
        match spec {
            CType::Primitive(sign, ty) => {
                if let Some(token) = self.sign_to_token(*sign) {
                    self.push_token(&token);
                }
                let (long, t) = self.ty_to_token(*ty);
                if long {
                    self.push_token(&Token::Long(0));
                }
                self.push_token(&t);
            }
            CType::TypeOf(ty) => self.type_of(ty),
            CType::Void => self.push_token(&Token::Void(0)),
            CType::Custom(ident) => self.push_token(&Token::Identifier(0, ident.clone())),

            CType::Compound(compound) => self.compound_type(compound),
        }
    }

    fn type_of(&mut self, ty: &TypeOf) {
        match ty {
            TypeOf::Expression(e) => {
                self.push_token(&Token::Identifier(0, From::from("typeof")));
                self.push_token(&Token::OpenParen(0));
                self.expression(&**e);
                self.push_token(&Token::CloseParen(0));
                self.push(" ");
            }
            TypeOf::TypeName(e) => {
                self.push_token(&Token::Identifier(0, From::from("typeof")));
                self.push_token(&Token::OpenParen(0));
                self.type_name(&**e);
                self.push_token(&Token::CloseParen(0));
                self.push(" ");
            }
        }
    }

    fn compound_type(&mut self, ty: &CompoundType) {
        match ty {
            CompoundType::Enum(name, enum_fields) => {
                self.enum_ty(Some(name), enum_fields.as_ref().map(Vec::as_slice))
            }
            CompoundType::AnonymousEnum(enum_fields) => self.enum_ty(None, Some(enum_fields)),
            CompoundType::Struct(name, struct_fields) => {
                self.push_token(&Token::Struct(0));
                self.struct_ty(Some(name), struct_fields.as_ref().map(Vec::as_slice))
            }
            CompoundType::AnonymousStruct(struct_fields) => {
                self.push_token(&Token::Struct(0));
                self.struct_ty(None, Some(struct_fields))
            }
            CompoundType::Union(name, struct_fields) => {
                self.push_token(&Token::Union(0));
                self.struct_ty(Some(name), struct_fields.as_ref().map(Vec::as_slice))
            }
            CompoundType::AnonymousUnion(struct_fields) => {
                self.push_token(&Token::Union(0));
                self.struct_ty(None, Some(struct_fields))
            }
        }
    }

    fn enum_ty(&mut self, name: Option<&Rc<str>>, fields: Option<&[EnumField]>) {
        self.push_token(&Token::Enum(0));
        if let Some(n) = name {
            self.push_token(&Token::Identifier(0, n.clone()));
        }
        if let Some(fields) = fields {
            self.push_token(&Token::OpenBrace(0));
            if let Some((last, rest)) = fields.split_last() {
                for field in rest {
                    self.enum_field(field);
                    self.whitespace = false;
                    self.push(",");
                    self.newline = true;
                }
                self.enum_field(last);
            }
            self.push_token(&Token::CloseBrace(0));
        }
    }

    fn enum_field(&mut self, field: &EnumField) {
        self.push_token(&Token::Identifier(0, field.0.clone()));
        if let Some(e) = &field.1 {
            self.push_token(&Token::Assign(0));
            self.ternary_expression(e);
        }
    }

    fn struct_ty(&mut self, name: Option<&Rc<str>>, fields: Option<&[StructField]>) {
        if let Some(n) = name {
            self.push_token(&Token::Identifier(0, n.clone()));
        }
        if let Some(fields) = fields {
            self.push_token(&Token::OpenBrace(0));
            for field in fields {
                self.declaration_specifiers(&*field.0);
                if let Some(declarators) = &field.1 {
                    if let Some((last, rest)) = declarators.split_last() {
                        for decl in rest {
                            self.struct_field(decl);
                            self.push_token(&Token::Comma(0));
                        }
                        self.struct_field(last);
                    }
                }
                self.whitespace = false;
                self.newline = false;
                self.push(";");
                self.newline = true;
            }
            self.push_token(&Token::CloseBrace(0));
        }
    }

    fn struct_field(&mut self, field: &(EitherDeclarator, Option<Box<GeneralExpression>>)) {
        match field {
            (EitherDeclarator::Anonymous(decl), bitfield) => {
                self.abstract_declarator(decl);
                if let Some(expr) = bitfield {
                    self.push_token(&Token::Colon(0));
                    self.general_expression(&**expr);
                }
            }
            (EitherDeclarator::Declarator(decl), bitfield) => {
                self.declarator(decl);
                if let Some(expr) = bitfield {
                    self.push_token(&Token::Colon(0));
                    self.general_expression(&**expr);
                }
            }
        }
    }

    fn storage_specifiers(&mut self, storage: &StorageClassSpecifiers) {
        if storage.typedef {
            self.push_token(&Token::Typedef(0));
        }
        if storage.extern_ {
            self.push_token(&Token::Extern(0));
        }
        if storage.static_ {
            self.push_token(&Token::Static(0));
        }
        if storage.inline {
            self.push_token(&Token::Inline(0));
        }
        if storage.auto {
            self.push_token(&Token::Auto(0));
        }
        if storage.register {
            self.push_token(&Token::Register(0));
        }
    }

    fn type_qualifiers(&mut self, ty: &TypeQualifiers) {
        if ty.const_ {
            self.push_token(&Token::Const(0));
        }
        if ty.volatile {
            self.push_token(&Token::Volatile(0));
        }
    }
}
