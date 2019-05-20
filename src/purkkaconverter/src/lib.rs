#![feature(box_patterns, drain_filter)]

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use cparser::grammar as cp;
use ctoken::token as ct;
use purkkasyntax as pp;
use purkkasyntax::{Param, TypeSignature};
use purkkatoken::token as pt;

mod traits;

mod array;
mod declarations;
mod imports;
mod inference;
mod lambda;

use traits::TreeTransformer;

use array::ArrayToPointer;
use declarations::FetchDeclarations;
use imports::StripImports;
use inference::TypeInferrer;
use lambda::StripLambda;

#[derive(Clone, Debug)]
pub struct Context {
    pub functions: HashMap<Rc<str>, pp::Lambda>,
    pub global_includes: HashSet<Rc<str>>,
    pub local_includes: HashSet<Rc<str>>,
}

pub fn convert(mut purkka_tree: pp::S) -> (cp::S, Context) {
    let mut context = Context::new();
    TypeInferrer::new(&mut context).transform(&mut purkka_tree);
    ArrayToPointer::new(&mut context).transform(&mut purkka_tree);
    StripLambda::new(&mut context).transform(&mut purkka_tree);
    StripImports::new(&mut context).transform(&mut purkka_tree);
    (context.s(purkka_tree), context)
}

pub fn fetch_identifiers_from_prk(tree: &mut pp::S) -> HashMap<Rc<str>, pp::Declaration> {
    let mut decls = FetchDeclarations::new();
    decls.fetch_declarations(tree);
    decls.declarations
}

pub fn fetch_identifiers_from_c(_tree: &mut cp::S) -> HashMap<Rc<str>, pp::Declaration> {
    panic!()
}

impl Context {
    fn new() -> Context {
        Context {
            functions: HashMap::new(),
            global_includes: HashSet::new(),
            local_includes: HashSet::new(),
        }
    }

    fn push_function(&mut self, name: Rc<str>, lambda: pp::Lambda) {
        if self.functions.contains_key(&name) {
            panic!("Duplicate function: {}", name);
        }
        self.functions.insert(name, lambda);
    }

    fn push_anonymous_function(&mut self, lambda: pp::Lambda) -> Rc<str> {
        let name: Rc<str> = From::from(format!("_lambda_{}", self.functions.len()));
        self.functions.insert(name.clone(), lambda);
        name
    }

    /* Output the C parse tree */

    pub fn s(&mut self, purkka_tree: pp::S) -> cp::S {
        match purkka_tree {
            pp::S::TranslationUnit(u) => cp::S::TranslationUnit(self.translation_unit(u)),
        }
    }

    pub fn translation_unit(&mut self, k: pp::TranslationUnit) -> cp::TranslationUnit {
        match k {
            pp::TranslationUnit::Units(u) => {
                let drain = self.functions.drain().collect::<Vec<_>>();
                let funcs = drain
                    .into_iter()
                    .map(|(n, u)| self.format_lambda_as_external_decl(n.clone(), u.clone()))
                    .collect::<Vec<_>>();
                cp::TranslationUnit::Units(
                    u.into_iter()
                        .map(|u| self.unit_to_external_decl(u))
                        .chain(funcs.into_iter())
                        .collect(),
                )
            }
        }
    }

    fn format_lambda_as_external_decl(
        &mut self,
        name: Rc<str>,
        pp::Lambda::Lambda(params, ty, block): pp::Lambda,
    ) -> cp::ExternalDeclaration {
        cp::ExternalDeclaration::FunctionDefinition(Box::new(
            cp::FunctionDefinition::FunctionDefinition(
                Some(Box::new(self.type_to_declaration_specifiers(ty.clone()))),
                vec![cp::Declarator::Declarator(
                    None,
                    Box::new(self.function_params_from_params(name.clone(), params)),
                )],
                Box::new(self.block_expression_to_compound_statement(block)),
            ),
        ))
    }

    pub fn cond_and_block_to_selection_statement(
        &mut self,
        cond: pp::Expression,
        block: pp::Block,
        otherwise: Option<Box<cp::Statement>>,
    ) -> cp::SelectionStatement {
        cp::SelectionStatement::If(
            Box::new(self.expression(cond)),
            cp::Statement::CompoundStatement(Box::new(cp::CompoundStatement::Statements(
                self.block_to_statement_list(block),
            ))),
            otherwise,
        )
    }

    pub fn block_expression_to_compound_statement(
        &mut self,
        block: pp::BlockExpression,
    ) -> cp::CompoundStatement {
        match block {
            pp::BlockExpression::If(arms, otherwise) => {
                let mut iter = arms.into_iter();
                let first = iter.next().unwrap();

                let else_block = otherwise
                    .map(|block| {
                        Some(cp::Statement::CompoundStatement(Box::new(
                            cp::CompoundStatement::Statements(
                                self.block_to_statement_list(*block),
                            ),
                        )))
                    })
                    .unwrap_or(None);

                let tail = iter.rev().fold(else_block, |prev, next| {
                    Some(cp::Statement::SelectionStatement(Box::new(
                        self.cond_and_block_to_selection_statement(*next.0, *next.1, prev.map(Box::new)),
                    )))
                });

                cp::CompoundStatement::Statements(vec![cp::StatementOrDeclaration::Statement(
                    cp::Statement::SelectionStatement(Box::new(
                        self.cond_and_block_to_selection_statement(*first.0, *first.1, tail.map(Box::new)),
                    )),
                )])
            }
            pp::BlockExpression::Block(block) => {
                cp::CompoundStatement::Statements(self.block_to_statement_list(block))
            }
            pp::BlockExpression::For(first, second, third, block, None) => {
                cp::CompoundStatement::Statements(vec![cp::StatementOrDeclaration::Statement(
                    cp::Statement::IterationStatement(Box::new(cp::IterationStatement::For(
                        ct::Token::For(0),
                        ct::Token::OpenParen(0),
                        self.expressions_to_for_expr(
                            first.map(|t| *t),
                            second.map(|t| *t),
                            third.map(|t| *t),
                        ),
                        ct::Token::CloseParen(0),
                        Box::new(self.block_to_statement(*block)),
                    ))),
                )])
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn block_to_statement_list(&mut self, block: pp::Block) -> Vec<cp::StatementOrDeclaration> {
        match block {
            pp::Block::Statements(statements) => statements
                .into_iter()
                .map(|next| self.statement_to_statement_or_declaration(next))
                .collect(),
        }
    }

    pub fn block_to_statement(&mut self, block: pp::Block) -> cp::Statement {
        cp::Statement::CompoundStatement(Box::new(cp::CompoundStatement::Statements(
            self.block_to_statement_list(block),
        )))
    }

    pub fn expressions_to_for_expr(
        &mut self,
        first: Option<pp::Statement>,
        second: Option<pp::Statement>,
        third: Option<pp::Statement>,
    ) -> cp::ForExpr {
        match third {
            Some(expr) => cp::ForExpr::ExpressionStatement(
                Box::new(self.statement_to_expression_statement(first)),
                Box::new(self.statement_to_expression_statement(second)),
                Box::new(self.statement_to_expression(expr)),
            ),
            None => cp::ForExpr::EmptyLast(
                Box::new(self.statement_to_expression_statement(first)),
                Box::new(self.statement_to_expression_statement(second)),
            ),
        }
    }

    pub fn statement_to_expression_statement(
        &mut self,
        expr: Option<pp::Statement>,
    ) -> cp::ExpressionStatement {
        cp::ExpressionStatement::Expression(expr.map(|e| Box::new(self.statement_to_expression(e))))
    }

    pub fn statement_to_expression(&mut self, expr: pp::Statement) -> cp::Expression {
        match expr {
            pp::Statement::Expression(e) => self.expression(*e),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn statement_to_statement_or_declaration(
        &mut self,
        statement: pp::Statement,
    ) -> cp::StatementOrDeclaration {
        match statement {
            pp::Statement::Declaration(decl) => {
                cp::StatementOrDeclaration::Declaration(self.convert_declaration(*decl))
            }
            pp::Statement::BlockExpression(block) => {
                cp::StatementOrDeclaration::Statement(cp::Statement::CompoundStatement(Box::new(
                    self.block_expression_to_compound_statement(*block),
                )))
            }
            pp::Statement::Expression(expr) => {
                cp::StatementOrDeclaration::Statement(cp::Statement::ExpressionStatement(Box::new(
                    cp::ExpressionStatement::Expression(Some(Box::new(self.expression(*expr)))),
                )))
            }
            pp::Statement::Return(Some(e)) => cp::StatementOrDeclaration::Statement(
                cp::Statement::JumpStatement(Box::new(cp::JumpStatement::Return(
                    ct::Token::Return(0),
                    self.expression(*e),
                    ct::Token::Semicolon(0),
                ))),
            ),
            pp::Statement::Return(None) => {
                cp::StatementOrDeclaration::Statement(cp::Statement::JumpStatement(Box::new(
                    cp::JumpStatement::ReturnVoid(ct::Token::Return(0), ct::Token::Semicolon(0)),
                )))
            }
        }
    }

    pub fn function_params_from_params(&mut self, name: Rc<str>, params: Vec<Param>) -> cp::DirectDeclarator {
        let e = Box::new(cp::DirectDeclarator::Identifier(name));
        cp::DirectDeclarator::Function(e, self.parameter_list_from_params(params))
    }

    pub fn parameter_list_from_params(&mut self, params: Vec<Param>) -> Vec<cp::FunctionParam> {
        params
            .into_iter()
            .map(|t| cp::FunctionParam::Parameter(self.param_to_declaration(t)))
            .collect()
    }

    pub fn param_to_declaration(&mut self, param: Param) -> cp::ParameterDeclaration {
        match param {
            Param::Param(name, ty) => {
                let decl_spec = self.type_to_declaration_specifiers(*ty.clone());
                let decl = self.format_decl(name, *ty);
                cp::ParameterDeclaration::Declarator(Box::new(decl_spec), Box::new(decl))
            }
            Param::Anon(_ty) => unimplemented!(),
        }
    }

    pub fn unit_to_external_decl(&mut self, k: pp::Unit) -> cp::ExternalDeclaration {
        match k {
            pp::Unit::Declaration(decl) => {
                cp::ExternalDeclaration::Declaration(Box::new(self.convert_declaration(*decl)))
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn convert_declaration(&mut self, k: pp::Declaration) -> cp::Declaration {
        match k {
            pp::Declaration::Declaration(false, _mutable, name, Some(ty), Some(expr)) => {
                cp::Declaration::Declaration(
                    Box::new(self.type_to_declaration_specifiers(*ty.clone())),
                    vec![cp::InitDeclarator::Assign(
                        Box::new(self.format_decl(name, *ty)),
                        ct::Token::Assign(0),
                        Box::new(cp::AssignmentOrInitializerList::AssignmentExpression(
                            self.assignment_expression(*expr),
                        )),
                    )],
                )
            }
            pp::Declaration::Declaration(false, _mutable, name, Some(ty), None) => {
                cp::Declaration::Declaration(
                    Box::new(self.type_to_declaration_specifiers(*ty.clone())),
                    vec![cp::InitDeclarator::Declarator(Box::new(
                        self.format_decl(name, *ty),
                    ))],
                )
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn type_to_declaration_specifiers(
        &mut self,
        ty: TypeSignature,
    ) -> cp::DeclarationSpecifiers {
        match ty {
            TypeSignature::Plain(ty) => {
                let c_ty = match ty.as_ref() {
                    "int" => cp::CType::Primitive(None, cp::PrimitiveType::Int),
                    "char" => cp::CType::Primitive(None, cp::PrimitiveType::Char),
                    _ => cp::CType::Custom(ty),
                };
                cp::DeclarationSpecifiers::DeclarationSpecifiers(None, Some(c_ty))
            }
            TypeSignature::Infer => cp::DeclarationSpecifiers::DeclarationSpecifiers(
                None,
                Some(cp::CType::Primitive(None, cp::PrimitiveType::Int)),
            ),
            TypeSignature::Array(ty, _) => self.type_to_declaration_specifiers(*ty),
            TypeSignature::Pointer { ty, .. } => self.type_to_declaration_specifiers(*ty),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn ty_to_pointer(&mut self, ty: TypeSignature) -> Option<Box<cp::Pointer>> {
        match ty {
            TypeSignature::Pointer { ty, .. } => Some(Box::new(cp::Pointer::Ptr(
                cp::TypeQualifiers::default(),
                self.ty_to_pointer(*ty),
            ))),
            _ => None,
        }
    }

    pub fn assignment_expression(&mut self, expr: pp::Expression) -> cp::AssignmentExpression {
        cp::AssignmentExpression::TernaryExpression(self.expression_as_ternary(expr))
    }

    pub fn expression(&mut self, k: pp::Expression) -> cp::Expression {
        cp::Expression::Expression(vec![cp::AssignmentExpression::TernaryExpression(
            self.expression_as_ternary(k),
        )])
    }

    pub fn expression_as_ternary(&mut self, k: pp::Expression) -> cp::TernaryExpression {
        match k {
            pp::Expression::PrimaryExpression(primary_expr) => {
                cp::TernaryExpression::GeneralExpression(cp::GeneralExpression::CastExpression(
                    Box::new(cp::CastExpression::UnaryExpression(
                        cp::UnaryExpression::PostfixExpression(Box::new(
                            cp::PostfixExpression::PrimaryExpression(
                                self.primary_expr(primary_expr),
                            ),
                        )),
                    )),
                ))
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn primary_expr(&mut self, k: pp::PrimaryExpression) -> cp::PrimaryExpression {
        match k {
            pp::PrimaryExpression::Literal(pp::Literal::Integer(pt::Token::Integer(_, i))) => {
                cp::PrimaryExpression::Number(From::from(i.to_string()))
            }
            pp::PrimaryExpression::Identifier(i) => {
                cp::PrimaryExpression::Identifier(i.clone())
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn format_decl(&mut self, name: Rc<str>, ty: TypeSignature) -> cp::Declarator {
        cp::Declarator::Declarator(
            self.ty_to_pointer(ty.clone()),
            Box::new(self.format_direct_decl(name, ty)),
        )
    }

    pub fn format_direct_decl(&mut self, name: Rc<str>, ty: TypeSignature) -> cp::DirectDeclarator {
        match ty {
            TypeSignature::Plain(_) => cp::DirectDeclarator::Identifier(name),
            TypeSignature::Pointer { ty, .. } => self.format_direct_decl(name, *ty),
            other => panic!("Not implemented: {:?}", other),
        }
    }
}
