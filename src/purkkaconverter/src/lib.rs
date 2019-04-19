use std::rc::Rc;

use cparser::parser as cp;
use ctoken::token as ct;
use purkkaparser::{parser as pp, token as pt};

struct Context {}

pub fn convert(purkka_tree: pp::S) -> cp::S {
    let context = Context {};
    context.s(purkka_tree)
}

impl Context {
    pub fn s(&self, purkka_tree: pp::S) -> cp::S {
        match purkka_tree {
            pp::S::TranslationUnit(u) => cp::S::TranslationUnit(self.translation_unit(u)),
        }
    }

    pub fn translation_unit(&self, k: pp::TranslationUnit) -> cp::TranslationUnit {
        match k {
            pp::TranslationUnit::Units(u) => {
                u.into_iter()
                    .fold(cp::TranslationUnit::Epsilon(), |tree, next| {
                        cp::TranslationUnit::TranslationUnit(
                            Box::new(tree),
                            self.unit_to_decl(next),
                        )
                    })
            }
        }
    }

    pub fn unit_to_decl(&self, k: pp::Unit) -> cp::ExternalDeclaration {
        match k {
            pp::Unit::Declaration(decl) => {
                if let pp::Declaration::Declaration(_public, _mutable, name, Some(ty), Some(expr)) =
                    decl
                {
                    cp::ExternalDeclaration::Declaration(Box::new(cp::Declaration::List(
                        cp::DeclarationSpecifiers::Neither(
                            cp::TypeSpecifier::Int(ct::Token::Int(0))),
                        cp::InitDeclaratorList::InitDeclarator(cp::InitDeclarator::Assign(
                            self.format_decl(name, ty),
                            ct::Token::Assign(0),
                            cp::AssignmentOrInitializerList::AssignmentExpression(
                                self.assignment_expression(expr),
                            ),
                        )),
                        ct::Token::Semicolon(0),
                    )))
                } else if let pp::Declaration::Declaration(_public, _mutable, name, Some(ty), None) =
                    decl
                {
                    cp::ExternalDeclaration::Declaration(Box::new(cp::Declaration::List(
                        cp::DeclarationSpecifiers::Neither(
                            cp::TypeSpecifier::Int(ct::Token::Int(0))),
                        cp::InitDeclaratorList::InitDeclarator(cp::InitDeclarator::Declarator(
                            self.format_decl(name, ty),
                        )),
                        ct::Token::Semicolon(0),
                    )))
                } else {
                    panic!("Not implemented: {:?}", decl)
                }
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn assignment_expression(&self, k: pp::Assignment) -> cp::AssignmentExpression {
        match k {
            pp::Assignment::Expression(expr) => {
                cp::AssignmentExpression::TernaryExpression(self.expression(expr))
            }
        }
    }

    pub fn expression(&self, k: pp::Expression) -> cp::TernaryExpression {
        match k {
            pp::Expression::PrimaryExpression(primary_expr) => {
                cp::TernaryExpression::GeneralExpression(cp::GeneralExpression::CastExpression(
                    Box::new(cp::CastExpression::UnaryExpression(
                        cp::UnaryExpression::PostfixExpression(
                            cp::PostfixExpression::PrimaryExpression(
                                self.primary_expr(primary_expr),
                            ),
                        ),
                    )),
                ))
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn primary_expr(&self, k: pp::PrimaryExpression) -> cp::PrimaryExpression {
        match k {
            pp::PrimaryExpression::Literal(pp::Literal::Integer(pt::Token::Integer(i))) => {
                cp::PrimaryExpression::Number(ct::Token::Number(0, i.to_string()))
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn format_decl(&self, name: Rc<str>, ty: pp::TypeSignature) -> cp::Declarator {
        if let pp::TypeSignature::Plain(_) = ty {
            cp::Declarator::Identifier(
                ct::Token::Identifier(0, name.to_string()),
                cp::DirectDeclarator::Epsilon(),
            )
        } else {
            panic!("Not implemented: {:?}", ty);
        }
    }
}
