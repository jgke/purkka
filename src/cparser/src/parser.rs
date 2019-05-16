use std::collections::HashSet;
use std::iter::Peekable;

use ctoken::token::Token;

use crate::*;
use crate::grammar::*;

macro_rules! maybe_read_token {
    ($iter:expr, $tok:path) => {
        if let Some($tok(..)) = $iter.peek() {
            $iter.next()
        } else {
            None
        }
    };
}

macro_rules! unexpected_token {
    ($token:expr, $iter:expr) => {
        match $token {
            None => panic!("Unexpected end of file"),
            Some(t) => {
                let as_s = format!("{:?}", t);
                let num = t.get_num();
                panic!(
                    "Unexpected token: {}\n{}",
                    as_s,
                    $iter.fragment.source_to_str(&$iter.sources[num])
                );
            }
        }
    };
}

macro_rules! unexpected_token_expected_one {
    ($token:expr, $iter:expr, $expected:path) => {
        match $token {
            None => panic!("Unexpected end of file, expected {}", stringify!($expected)),
            Some(t) => {
                let as_s = format!("{:?}", t);
                let expected = stringify!($expected);
                let num = t.get_num();
                panic!(
                    "Unexpected token: {}, expected {}\n{}",
                    as_s,
                    expected,
                    $iter.fragment.source_to_str(&$iter.sources[num])
                );
            }
        }
    };
}

macro_rules! read_token {
    ($iter:expr, $tok:path) => {
        if let Some($tok(..)) = $iter.peek() {
            $iter.next().unwrap().clone()
        } else {
            unexpected_token_expected_one!(&$iter.next(), $iter, $tok)
        }
    };
}

#[derive(Debug)]
pub struct ScopedState {
    pub types: HashSet<String>,
    pub labels: HashSet<String>,
}

#[derive(Debug)]
pub struct ParseContext<'a, 'b> {
    pub scope: Vec<ScopedState>,
    pub iter: Iter<'a, 'b>,
    pub fragment: &'a FragmentIterator,
    pub sources: &'a [Source],
}

fn default_types() -> ScopedState {
    let mut types = HashSet::new();
    
    types.insert("va_list".to_string());
    types.insert("__builtin_va_list".to_string());
    types.insert("size_t".to_string());
    types.insert("_Bool".to_string());
    types.insert("_Complex".to_string());

    ScopedState {
        types,
        labels: HashSet::new(),
    }
}

pub(crate) type Iter<'a, 'b> = &'a mut Peekable<std::slice::Iter<'b, Token>>;

pub fn parse(iter: Iter, sources: &[Source], fragment_iter: &FragmentIterator) -> Result<S, Option<Token>> {
    let mut context = ParseContext {
        scope: vec![default_types()],
        iter,
        fragment: fragment_iter,
        sources,
    };

    Ok(S::TranslationUnit(context.parse_translation_unit()))
}

impl<'a, 'b> ParseContext<'a, 'b> {
    fn next(&mut self) -> Option<&'a Token> {
        self.iter.next()
    }
    fn peek(&mut self) -> Option<&&'a Token> {
        self.iter.peek()
    }

    fn parse_translation_unit(&mut self) -> TranslationUnit {
        let mut units = Vec::new();
        while maybe_read_token!(self, Token::Semicolon).is_some() {}
        while self.peek().is_some() {
            units.push(self.parse_external_declaration());
            while maybe_read_token!(self, Token::Semicolon).is_some() {}
        }
        TranslationUnit::Units(units)
    }

    fn parse_external_declaration(&mut self) -> ExternalDeclaration {
        let _typedef = maybe_read_token!(self, Token::Typedef).is_some();
        let specifiers = Box::new(self.parse_declaration_specifiers());
        let declarator = self.parse_declarator();
        match self.peek() {
            Some(Token::OpenBrace(..)) => {
                let block = Box::new(self.parse_compound_statement());
                ExternalDeclaration::FunctionDefinition(Box::new(FunctionDefinition::FunctionDefinition(Some(specifiers), vec![declarator], block)))
            }
            Some(Token::Semicolon(..)) => unimplemented!(),
            _ => ExternalDeclaration::Declaration(Box::new(Declaration::Declaration(specifiers, self.parse_init_declarator_list(declarator))))
        }
    }

    fn parse_declaration_specifiers(&mut self) -> DeclarationSpecifiers {
        let mut spec = (StorageClassSpecifiers::default(), TypeQualifiers::default());
        self.parse_specifiers(&mut spec);
        let ty = self.parse_type_specifier();
        self.parse_specifiers(&mut spec);

        let opt_spec = if spec.0.any() || spec.1.any() {
            Some(spec)
        } else {
            None
        };

        DeclarationSpecifiers::DeclarationSpecifiers(opt_spec, ty)
    }

    fn parse_specifiers(&mut self, spec: &mut Specifiers) {
        loop {
            match self.peek() {
                Some(Token::Extern(..)) => spec.0.extern_ = true,
                Some(Token::Static(..)) => spec.0.static_ = true,
                Some(Token::Inline(..)) => spec.0.inline = true,
                Some(Token::Auto(..)) => spec.0.auto = true,
                Some(Token::Register(..)) => spec.0.register = true,
                Some(Token::Const(..)) => spec.1.const_ = true,
                Some(Token::Volatile(..)) => spec.1.volatile = true,
                _ => break
            }
            self.next();
        }
    }

    fn parse_type_specifier(&mut self) -> Option<TypeSpecifier> {
        match self.peek().cloned() {
            Some(Token::Void(..)) => {
                self.next();
                Some(CType::Void)
            }
            Some(Token::Int(..)) => {
                self.next();
                Some(CType::Primitive(None, PrimitiveType::Int))
            }
            Some(Token::Signed(..)) | Some(Token::Unsigned(..)) => unimplemented!(),
            Some(Token::Enum(..)) | Some(Token::Struct(..)) => unimplemented!(),
            Some(Token::Float(..)) | Some(Token::Double(..)) => unimplemented!(),
            Some(Token::Identifier(_, ident)) if self.is_type(ident) => {
                self.next();
                Some(CType::Custom(ident.clone()))
            }
            t => unexpected_token!(t, self),
        }
    }

    fn parse_declarator(&mut self) -> Declarator {
        let ptr = self.parse_ptr();
        match self.peek() {
            Some(Token::Identifier(_, s)) => {
                read_token!(self, Token::Identifier);
                Declarator::Declarator(ptr, s.clone(), self.parse_direct_declarator())
            }
            Some(Token::OpenParen(..)) => {
                read_token!(self, Token::OpenParen);
                let decl = Box::new(self.parse_declarator());
                read_token!(self, Token::CloseParen);
                Declarator::FunctionPointer(ptr, decl, self.parse_direct_declarator())
            }
            t => unexpected_token!(t, self),
        }
    }

    fn parse_ptr(&mut self) -> Option<Box<Pointer>> {
        match self.peek() {
            Some(Token::Times(..)) => {
                let mut spec = TypeQualifiers::default();
                loop {
                    match self.peek() {
                        Some(Token::Const(..)) => spec.const_ = true,
                        Some(Token::Volatile(..)) => spec.volatile = true,
                        _ => break
                    }
                }
                Some(Box::new(Pointer::Ptr(spec, self.parse_ptr())))
            }
            _ => None
        }
    }

    fn parse_direct_declarator(&mut self) -> Box<DirectDeclarator> {
        let mut decl = Box::new(DirectDeclarator::Nothing);
        loop {
            match self.peek() {
                Some(Token::OpenBracket(..)) => {
                    read_token!(self, Token::OpenBracket);
                    let expr = match self.peek() {
                        Some(Token::CloseBracket(..)) => None,
                        _ => Some(Box::new(self.parse_general_expr())),
                    };
                    read_token!(self, Token::CloseBracket);
                    decl = Box::new(DirectDeclarator::ArrayOf(decl, expr));
                }
                Some(Token::OpenParen(..)) => {
                    read_token!(self, Token::OpenParen);
                    let params = self.parse_params();
                    read_token!(self, Token::CloseParen);
                    decl = Box::new(DirectDeclarator::Function(decl, params));
                }
                _ => break
            }
        }
        decl
    }

    fn parse_general_expr(&mut self) -> GeneralExpression {
        panic!()
    }

    fn parse_params(&mut self) -> Vec<FunctionParam> {
        let params = Vec::new();

        loop {
            match self.peek() {
                Some(Token::CloseParen(..)) => break,
                t => unexpected_token!(t, self),
            }
        }

        params
    }

    fn parse_init_declarator_list(&mut self, declarator: Declarator) -> Vec<InitDeclarator> {
        let mut decls = vec![self.parse_init_declarator(declarator)];
        while let Some(Token::Comma(..)) = self.peek() {
            read_token!(self, Token::Comma);
            let decl = self.parse_declarator();
            decls.push(self.parse_init_declarator(decl));
        }
        decls
    }

    fn parse_init_declarator(&mut self, decl: Declarator) -> InitDeclarator {
        match self.peek() {
            Some(Token::Asm(..)) => unimplemented!(),
            Some(Token::Assign(..)) => {
                let t = read_token!(self, Token::Assign);
                let list = self.parse_assignment_or_initializer_list();
                InitDeclarator::Assign(Box::new(decl), t, Box::new(list))
            }
            _ => InitDeclarator::Declarator(Box::new(decl))
        }
    }

    fn parse_assignment_or_initializer_list(&mut self) -> AssignmentOrInitializerList {
        match self.peek() {
            Some(Token::OpenBrace(..)) => {
                read_token!(self, Token::OpenBrace);

                if let Some(Token::CloseBrace(..)) = self.peek() {
                    read_token!(self, Token::CloseBrace);
                    return AssignmentOrInitializerList::Initializers(vec![]);
                }

                let mut initializers = vec![self.parse_initializer()];

                loop {
                    match self.peek() {
                        Some(Token::Comma(..)) => {
                            read_token!(self, Token::Comma);

                            if let Some(Token::CloseBrace(..)) = self.peek() {
                                break
                            }

                            initializers.push(self.parse_initializer());
                        }
                        _ => break
                    }
                }
                dbg!("wat");

                read_token!(self, Token::CloseBrace);

                AssignmentOrInitializerList::Initializers(initializers)
            }

            _ => AssignmentOrInitializerList::AssignmentExpression(self.parse_assignment_expression())
        }
    }

    fn parse_initializer(&mut self) -> Initializer {
        match self.peek() {
            Some(Token::Dot(..)) => {
                read_token!(self, Token::Dot);
                let ident = read_token!(self, Token::Identifier);
                read_token!(self, Token::Assign);
                let list = self.parse_assignment_or_initializer_list();
                if let Token::Identifier(_, ident) = ident {
                    Initializer::Initializer(Some(ident.clone()), Box::new(list))
                } else {
                    unreachable!()
                }
            }
            _ => Initializer::Initializer(None, Box::new(self.parse_assignment_or_initializer_list()))
        }
    }

    fn parse_compound_statement(&mut self) -> CompoundStatement {
        let mut stmts = Vec::new();
        read_token!(self, Token::OpenBrace);
        loop {
            match self.peek() {
                Some(Token::CloseBrace(..)) => break,
                _ => stmts.push(self.parse_stmt_or_declaration())
            }
        }
        read_token!(self, Token::CloseBrace);
        CompoundStatement::Statements(stmts)
    }

    #[allow(unreachable_patterns)]
    fn parse_stmt_or_declaration(&mut self) -> StatementOrDeclaration {
        if let Some(Token::Identifier(_, ident)) = self.peek() {
            if !self.is_type(&ident.clone()) {
                return StatementOrDeclaration::Statement(self.parse_statement());
            }
        }
        match_first!(
            self.peek() => _t,
            default unexpected_token!(_t, self),

            Declaration => StatementOrDeclaration::Declaration(self.parse_declaration()), 
            Statement => StatementOrDeclaration::Statement(self.parse_statement()), 
        )
    }

    fn parse_declaration(&mut self) -> Declaration {
        let spec = self.parse_declaration_specifiers();
        let init_list = match self.peek() {
            Some(Token::Semicolon(..)) => vec![],
            _ => {
                let decl = self.parse_declarator();
                self.parse_init_declarator_list(decl)
            }
        };
        read_token!(self, Token::Semicolon);
        Declaration::Declaration(Box::new(spec), init_list)
    }

    fn parse_statement(&mut self) -> Statement {
        match_first!(
            self.peek() => _t,
            default unexpected_token!(_t, self),

            CompoundStatement => Statement::CompoundStatement(Box::new(self.parse_compound_statement())),
            SelectionStatement => Statement::SelectionStatement(Box::new(self.parse_selection_statement())),
            IterationStatement => Statement::IterationStatement(Box::new(self.parse_iteration_statement())),
            JumpStatement => Statement::JumpStatement(Box::new(self.parse_jump_statement())),
            ExpressionStatement => {
                let e = Statement::ExpressionStatement(Box::new(ExpressionStatement::Expression(Some(Box::new(self.parse_expression())))));
                read_token!(self, Token::Semicolon);
                e
            },
        )
    }

    fn parse_selection_statement(&mut self) -> SelectionStatement {
        unimplemented!();
    }

    fn parse_iteration_statement(&mut self) -> IterationStatement {
        unimplemented!();
    }

    fn parse_jump_statement(&mut self) -> JumpStatement {
        unimplemented!();
    }

    fn is_type(&self, ident: &str) -> bool {
        self.scope.iter().any(|e| e.types.contains(ident))
    }

    fn parse_expression(&mut self) -> Expression {
        let mut exprs = vec![self.parse_assignment_expression()];
        loop {
            match self.peek() {
                Some(Token::Comma(..)) => {
                    read_token!(self, Token::Comma);
                    exprs.push(self.parse_assignment_expression());
                }
                _ => break
            }
        }
        Expression::Expression(exprs)
    }

    fn parse_assignment_expression(&mut self) -> AssignmentExpression {
        let left = self.parse_ternary_expression();
        match self.peek().unwrap() {
            Token::Assign(..) | Token::TimesAssign(..) | Token::DivAssign(..) | Token::ModAssign(..)
                | Token::PlusAssign(..) | Token::MinusAssign(..) | Token::BitShiftLeftAssign(..) | Token::BitShiftRightAssign(..)
                | Token::BitAndAssign(..) | Token::BitXorAssign(..) | Token::BitOrAssign(..)
                => {
                    if let TernaryExpression::GeneralExpression(GeneralExpression::CastExpression(box CastExpression::UnaryExpression(u))) = left {
                        let op = self.next();
                        let right = self.parse_assignment_expression();
                        AssignmentExpression::Assignment(Box::new(u), self.assignment_operator(op.unwrap().clone()), Box::new(right))
                    } else {
                        panic!("Tried to assign to a rvalue")
                    }
                }
            _ => AssignmentExpression::TernaryExpression(left)
        }
    }
    fn parse_ternary_expression(&mut self) -> TernaryExpression {
        let e = self.parse_general_expression();
        if let Some(Token::Ternary(..)) = self.peek() {
            let t1 = read_token!(self, Token::Ternary);
            let if_true = self.parse_expression();
            let t2 = read_token!(self, Token::Ternary);
            let otherwise = self.parse_ternary_expression();
            TernaryExpression::Ternary(*e, t1, Box::new(if_true), t2, Box::new(otherwise))
        } else {
            TernaryExpression::GeneralExpression(*e)
        }
    }

    fn parse_general_expression(&mut self) -> Box<GeneralExpression> {
        let left = self.parse_cast_expression();
        self.parse_general_expression_(1, Box::new(GeneralExpression::CastExpression(Box::new(left))))
    }

    /* 
    | 12: Times. GeneralExpression #Token::Times GeneralExpression
    | 12: Divide. GeneralExpression #Token::Divide GeneralExpression
    | 12: Mod. GeneralExpression #Token::Mod GeneralExpression
    | 11: Plus. GeneralExpression #Token::Plus GeneralExpression
    | 11: Minus. GeneralExpression #Token::Minus GeneralExpression
    | 10: BitShiftLeft. GeneralExpression #Token::BitShiftLeft GeneralExpression
    | 10: BitShiftRight. GeneralExpression #Token::BitShiftRight GeneralExpression
    | 9: LessThan. GeneralExpression #Token::LessThan GeneralExpression
    | 9: MoreThan. GeneralExpression #Token::MoreThan GeneralExpression
    | 9: LessEqThan. GeneralExpression #Token::LessEqThan GeneralExpression
    | 9: MoreEqThan. GeneralExpression #Token::MoreEqThan GeneralExpression
    | 8: Equals. GeneralExpression #Token::Equals GeneralExpression
    | 8: NotEquals. GeneralExpression #Token::NotEquals GeneralExpression
    | 7: BitAnd. GeneralExpression #Token::BitAnd GeneralExpression
    | 6: BitXor. GeneralExpression #Token::BitXor GeneralExpression
    | 5: BitOr. GeneralExpression #Token::BitOr GeneralExpression
    | 4: And. GeneralExpression #Token::And GeneralExpression
    | 3: Or. GeneralExpression #Token::Or GeneralExpression
    */

    fn parse_general_expression_(&mut self, priority: usize, mut left: Box<GeneralExpression>) -> Box<GeneralExpression> {
        loop {
            match self.peek() {
                Some(Token::Times(..)) if priority == 12 => { left = Box::new(GeneralExpression::Times(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::Times(..)) if priority < 12 => { left = self.parse_general_expression_(12, left); }
                Some(Token::Divide(..)) if priority == 12 => { left = Box::new(GeneralExpression::Divide(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::Divide(..)) if priority < 12 => { left = self.parse_general_expression_(12, left); }
                Some(Token::Mod(..)) if priority == 12 => { left = Box::new(GeneralExpression::Mod(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::Mod(..)) if priority < 12 => { left = self.parse_general_expression_(12, left); }
                Some(Token::Plus(..)) if priority == 11 => { left = Box::new(GeneralExpression::Plus(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::Plus(..)) if priority < 11 => { left = self.parse_general_expression_(11, left); }
                Some(Token::Minus(..)) if priority == 11 => { left = Box::new(GeneralExpression::Minus(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::Minus(..)) if priority < 11 => { left = self.parse_general_expression_(11, left); }
                Some(Token::BitShiftLeft(..)) if priority == 10 => { left = Box::new(GeneralExpression::BitShiftLeft(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::BitShiftLeft(..)) if priority < 10 => { left = self.parse_general_expression_(10, left); }
                Some(Token::BitShiftRight(..)) if priority == 10 => { left = Box::new(GeneralExpression::BitShiftRight(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::BitShiftRight(..)) if priority < 10 => { left = self.parse_general_expression_(10, left); }
                Some(Token::LessThan(..)) if priority == 9 => { left = Box::new(GeneralExpression::LessThan(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::LessThan(..)) if priority < 9 => { left = self.parse_general_expression_(9, left); }
                Some(Token::MoreThan(..)) if priority == 9 => { left = Box::new(GeneralExpression::MoreThan(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::MoreThan(..)) if priority < 9 => { left = self.parse_general_expression_(9, left); }
                Some(Token::LessEqThan(..)) if priority == 9 => { left = Box::new(GeneralExpression::LessEqThan(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::LessEqThan(..)) if priority < 9 => { left = self.parse_general_expression_(9, left); }
                Some(Token::MoreEqThan(..)) if priority == 9 => { left = Box::new(GeneralExpression::MoreEqThan(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::MoreEqThan(..)) if priority < 9 => { left = self.parse_general_expression_(9, left); }
                Some(Token::Equals(..)) if priority == 8 => { left = Box::new(GeneralExpression::Equals(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::Equals(..)) if priority < 8 => { left = self.parse_general_expression_(8, left); }
                Some(Token::NotEquals(..)) if priority == 8 => { left = Box::new(GeneralExpression::NotEquals(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::NotEquals(..)) if priority < 8 => { left = self.parse_general_expression_(8, left); }
                Some(Token::BitAnd(..)) if priority == 7 => { left = Box::new(GeneralExpression::BitAnd(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::BitAnd(..)) if priority < 7 => { left = self.parse_general_expression_(7, left); }
                Some(Token::BitXor(..)) if priority == 6 => { left = Box::new(GeneralExpression::BitXor(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::BitXor(..)) if priority < 6 => { left = self.parse_general_expression_(6, left); }
                Some(Token::BitOr(..)) if priority == 5 => { left = Box::new(GeneralExpression::BitOr(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::BitOr(..)) if priority < 5 => { left = self.parse_general_expression_(5, left); }
                Some(Token::And(..)) if priority == 4 => { left = Box::new(GeneralExpression::And(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::And(..)) if priority < 4 => { left = self.parse_general_expression_(4, left); }
                Some(Token::Or(..)) if priority == 3 => { left = Box::new(GeneralExpression::Or(left, self.next().cloned().unwrap(), Box::new(GeneralExpression::CastExpression(Box::new(self.parse_cast_expression()))))); }
                Some(Token::Or(..)) if priority < 3 => { left = self.parse_general_expression_(3, left); }
                _ => break
            }
        }
        left
    }

    fn parse_cast_expression(&mut self) -> CastExpression {
        CastExpression::UnaryExpression(self.parse_unary_expression())
    }

    fn parse_unary_expression(&mut self) -> UnaryExpression {
        match self.peek() {
            Some(Token::Increment(..)) | Some(Token::Decrement(..)) => {
                let t = self.next().cloned().unwrap();
                let op = self.increment_or_decrement(t);
                UnaryExpression::IncrementOrDecrement(op, Box::new(self.parse_unary_expression()))
            }
            Some(Token::BitAnd(..)) | Some(Token::Times(..)) | Some(Token::Plus(..))
                | Some(Token::Minus(..)) | Some(Token::BitNot(..)) | Some(Token::Not(..)) => {
                    let t = self.next().cloned().unwrap();
                    let op = self.unary_operator(t);
                    UnaryExpression::UnaryOperator(op, Box::new(self.parse_cast_expression()))
            }
            _ => UnaryExpression::PostfixExpression(Box::new(self.parse_postfix_expression()))
        }
    }

    fn parse_postfix_expression(&mut self) -> PostfixExpression {
        let e = self.parse_primary_expression();
        match self.peek() {
            Some(Token::OpenBracket(..)) => unimplemented!(),
            Some(Token::OpenParen(..)) => unimplemented!(),
            Some(Token::Dot(..)) | Some(Token::Arrow(..)) => unimplemented!(),
            Some(Token::Increment(..)) | Some(Token::Decrement(..)) => unimplemented!(),
            _ => PostfixExpression::PrimaryExpression(e)
        }
    }

    fn parse_primary_expression(&mut self) -> PrimaryExpression {
        match self.peek() {
            Some(Token::Identifier(..)) => PrimaryExpression::Identifier(self.next().cloned().unwrap()),
            Some(Token::Number(..)) => PrimaryExpression::Number(self.next().cloned().unwrap()),
            Some(Token::StringLiteral(..)) => PrimaryExpression::StringLiteral(self.next().cloned().unwrap()),
            Some(Token::CharLiteral(..)) => PrimaryExpression::CharLiteral(self.next().cloned().unwrap()),
            Some(Token::Sizeof(..)) => PrimaryExpression::Sizeof(self.next().cloned().unwrap()),
            Some(Token::Asm(..)) => PrimaryExpression::Asm(self.next().cloned().unwrap()),
            Some(Token::And(..)) => unimplemented!(),
            Some(Token::OpenParen(..)) => {
                let op = read_token!(self, Token::OpenParen);
                if let Some(Token::OpenBrace(..)) = self.peek() {
                    let compound = self.parse_compound_statement();
                    let cp = read_token!(self, Token::CloseParen);
                    PrimaryExpression::Statement(op, Box::new(compound), cp)
                } else {
                    let e = Box::new(self.parse_expression());
                    let cp = read_token!(self, Token::CloseParen);
                    PrimaryExpression::Expression(op, e, cp)
                }
            }
            t => unexpected_token!(t, self),
        }
    }

    fn assignment_operator(&self, token: Token) -> AssignmentOperator {
        match token {
            Token::Assign(..) => AssignmentOperator::Assign(token),
            Token::TimesAssign(..) => AssignmentOperator::TimesAssign(token),
            Token::DivAssign(..) => AssignmentOperator::DivAssign(token),
            Token::ModAssign(..) => AssignmentOperator::ModAssign(token),
            Token::PlusAssign(..) => AssignmentOperator::PlusAssign(token),
            Token::MinusAssign(..) => AssignmentOperator::MinusAssign(token),
            Token::BitShiftLeftAssign(..) => AssignmentOperator::BitShiftLeftAssign(token),
            Token::BitShiftRightAssign(..) => AssignmentOperator::BitShiftRightAssign(token),
            Token::BitAndAssign(..) => AssignmentOperator::BitAndAssign(token),
            Token::BitXorAssign(..) => AssignmentOperator::BitXorAssign(token),
            Token::BitOrAssign(..) => AssignmentOperator::BitOrAssign(token),
            _ => unreachable!()
        }
    }

    fn increment_or_decrement(&self, token: Token) -> IncrementOrDecrement {
        match token {
            Token::Increment(..) => IncrementOrDecrement::Increment(token),
            Token::Decrement(..) => IncrementOrDecrement::Decrement(token),
            _ => unreachable!()
        }
    }

    fn unary_operator(&self, token: Token) -> UnaryOperator {
        match token {
            Token::BitAnd(..) => UnaryOperator::BitAnd(token),
            Token::Times(..) => UnaryOperator::Times(token),
            Token::Plus(..) => UnaryOperator::Plus(token),
            Token::Minus(..) => UnaryOperator::Minus(token),
            Token::BitNot(..) => UnaryOperator::BitNot(token),
            Token::Not(..) => UnaryOperator::Not(token),
            _ => unreachable!()
        }
    }
}
