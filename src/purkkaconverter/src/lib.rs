#![feature(box_patterns, drain_filter)]

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use cparser::parser as cp;
use ctoken::token as ct;
use purkkasyntax as pp;
use purkkasyntax::{Param, TypeSignature};
use purkkatoken::token as pt;

mod traits;

mod array;
mod declarations;
mod imports;
mod lambda;

use traits::TreeTransformer;

use array::ArrayToPointer;
use declarations::FetchDeclarations;
use imports::StripImports;
use lambda::StripLambda;

#[derive(Clone, Debug)]
pub struct Context {
    pub functions: HashMap<Rc<str>, pp::Lambda>,
    pub global_includes: HashSet<Rc<str>>,
    pub local_includes: HashSet<Rc<str>>,
}

pub fn convert(mut purkka_tree: pp::S) -> (cp::S, Context) {
    let mut context = Context::new();
    ArrayToPointer::new(&mut context).transform(&mut purkka_tree);
    StripLambda::new(&mut context).transform(&mut purkka_tree);
    StripImports::new(&mut context).transform(&mut purkka_tree);
    dbg!(&context);
    dbg!(&purkka_tree);
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
                u.into_iter()
                    .map(|u| self.unit_to_external_decl(u))
                    .chain(funcs.into_iter())
                    .fold(cp::TranslationUnit::Epsilon(), |tree, next| {
                        cp::TranslationUnit::TranslationUnit(Box::new(tree), next)
                    })
            }
        }
    }

    fn format_lambda_as_external_decl(
        &mut self,
        name: Rc<str>,
        pp::Lambda::Lambda(params, ty, block): pp::Lambda,
    ) -> cp::ExternalDeclaration {
        cp::ExternalDeclaration::FunctionDefinition(Box::new(cp::FunctionDefinition::Specifiers(
            Box::new(self.type_to_declaration_specifiers(ty.clone())),
            Box::new(cp::Declarator::Identifier(
                ct::Token::Identifier(0, name.clone()),
                Box::new(self.function_params_from_params(params)),
            )),
            Box::new(self.block_expression_to_compound_statement(block)),
        )))
    }

    pub fn block_expression_to_compound_statement(
        &mut self,
        block: pp::BlockExpression,
    ) -> cp::CompoundStatement {
        match block {
            pp::BlockExpression::If(_arms, _otherwise) => unimplemented!(),
            pp::BlockExpression::Block(block) => cp::CompoundStatement::PushScope(
                cp::PushScope::OpenBrace(ct::Token::OpenBrace(0)),
                Box::new(self.block_to_statement_list(block)),
                ct::Token::CloseBrace(0),
            ),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn block_to_statement_list(&mut self, block: pp::Block) -> cp::StatementList {
        match block {
            pp::Block::Statements(statements) => {
                statements
                    .into_iter()
                    .fold(cp::StatementList::Epsilon(), |tree, next| {
                        cp::StatementList::More(
                            Box::new(tree),
                            self.statement_to_statement_or_declaration(next),
                        )
                    })
            }
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
            pp::Statement::Expression(expr) => cp::StatementOrDeclaration::Statement(
                cp::Statement::ExpressionStatement(Box::new(cp::ExpressionStatement::Expression(
                    Box::new(self.expression(*expr)),
                    ct::Token::Semicolon(0),
                ))),
            ),
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

    pub fn function_params_from_params(&mut self, params: Vec<Param>) -> cp::DirectDeclarator {
        let e = Box::new(cp::DirectDeclarator::Epsilon());
        let op = ct::Token::OpenParen(0);
        let cp = ct::Token::CloseParen(0);
        if params.is_empty() {
            cp::DirectDeclarator::Function(e, op, cp)
        } else {
            cp::DirectDeclarator::FunctionParams(
                e,
                op,
                cp::FunctionParams::ParameterTypeList(Box::new(
                    self.parameter_list_from_params(params),
                )),
                cp,
            )
        }
    }

    pub fn parameter_list_from_params(&mut self, params: Vec<Param>) -> cp::ParameterTypeList {
        let mut iter = params.into_iter();
        let first = iter.next().unwrap();
        cp::ParameterTypeList::ParameterList(iter.fold(
            cp::ParameterList::ParameterDeclaration(self.param_to_declaration(first)),
            |tree, next| {
                cp::ParameterList::ParameterList(
                    Box::new(tree),
                    ct::Token::Comma(0),
                    self.param_to_declaration(next),
                )
            },
        ))
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
                cp::Declaration::List(
                    Box::new(self.type_to_declaration_specifiers(*ty.clone())),
                    Box::new(cp::InitDeclaratorList::InitDeclarator(Box::new(
                        cp::InitDeclarator::Assign(
                            Box::new(self.format_decl(name, *ty)),
                            ct::Token::Assign(0),
                            Box::new(cp::AssignmentOrInitializerList::AssignmentExpression(
                                self.assignment_expression(*expr),
                            )),
                        ),
                    ))),
                    ct::Token::Semicolon(0),
                )
            }
            pp::Declaration::Declaration(false, _mutable, name, Some(ty), None) => {
                cp::Declaration::List(
                    Box::new(self.type_to_declaration_specifiers(*ty.clone())),
                    Box::new(cp::InitDeclaratorList::InitDeclarator(Box::new(
                        cp::InitDeclarator::Declarator(Box::new(self.format_decl(name, *ty))),
                    ))),
                    ct::Token::Semicolon(0),
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
                    "int" => cp::TypeSpecifier::Int(ct::Token::Int(0)),
                    "char" => cp::TypeSpecifier::Char(ct::Token::Char(0)),
                    other => panic!("Not implemented: {:?}", other),
                };
                cp::DeclarationSpecifiers::Neither(Box::new(c_ty))
            }
            TypeSignature::Infer => cp::DeclarationSpecifiers::Neither(Box::new(
                cp::TypeSpecifier::Int(ct::Token::Int(0)),
            )),
            TypeSignature::Array(ty, _) => self.type_to_declaration_specifiers(*ty),
            TypeSignature::Pointer { ty, .. } => self.type_to_declaration_specifiers(*ty),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn ty_to_pointer(&mut self, ty: TypeSignature) -> cp::Pointer {
        match ty {
            TypeSignature::Pointer { ty, .. } => {
                cp::Pointer::Pointer(ct::Token::Times(0), Box::new(self.ty_to_pointer(*ty)))
            }
            _ => cp::Pointer::Times(ct::Token::Times(0)),
        }
    }

    pub fn assignment_expression(&mut self, expr: pp::Expression) -> cp::AssignmentExpression {
        cp::AssignmentExpression::TernaryExpression(self.expression_as_ternary(expr))
    }

    pub fn expression(&mut self, k: pp::Expression) -> cp::Expression {
        cp::Expression::AssignmentExpression(Box::new(cp::AssignmentExpression::TernaryExpression(
            self.expression_as_ternary(k),
        )))
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
                cp::PrimaryExpression::Number(ct::Token::Number(0, From::from(i.to_string())))
            }
            pp::PrimaryExpression::Identifier(i) => {
                cp::PrimaryExpression::Identifier(ct::Token::Identifier(0, i.clone()))
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn format_decl(&mut self, name: Rc<str>, ty: TypeSignature) -> cp::Declarator {
        let ident = ct::Token::Identifier(0, name.clone());
        match ty {
            TypeSignature::Plain(_) => {
                cp::Declarator::Identifier(ident, Box::new(self.format_direct_decl(ty)))
            }
            TypeSignature::Pointer { ty, .. } => cp::Declarator::Pointer(
                Box::new(self.ty_to_pointer(*ty.clone())),
                cp::IdentifierOrType::Identifier(ident),
                Box::new(self.format_direct_decl(*ty)),
            ),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn format_direct_decl(&mut self, ty: TypeSignature) -> cp::DirectDeclarator {
        match ty {
            TypeSignature::Plain(_) => cp::DirectDeclarator::Epsilon(),
            TypeSignature::Pointer { ty, .. } => self.format_direct_decl(*ty),
            other => panic!("Not implemented: {:?}", other),
        }
    }
}
