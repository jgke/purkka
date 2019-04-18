use std::rc::Rc;

use cparser::parser as cp;
use ctoken::token as ct;
use kieliparser::{parser as kp, token as kt};

struct Context {}

pub fn convert(kieli_tree: kp::S) -> cp::S {
    let context = Context {};
    context.s(kieli_tree)
}

impl Context {
    pub fn s(&self, kieli_tree: kp::S) -> cp::S {
        match kieli_tree {
            kp::S::TranslationUnit(u) => cp::S::TranslationUnit(self.translation_unit(u)),
        }
    }

    pub fn translation_unit(&self, k: kp::TranslationUnit) -> cp::TranslationUnit {
        match k {
            kp::TranslationUnit::Units(u) => {
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

    pub fn unit_to_decl(&self, k: kp::Unit) -> cp::ExternalDeclaration {
        match k {
            kp::Unit::Declaration(decl) => {
                if let kp::Declaration::Declaration(public, mutable, name, Some(ty), Some(expr)) =
                    decl
                {
                    cp::ExternalDeclaration::Declaration(Box::new(cp::Declaration::List(
                        cp::DeclarationSpecifiers::TypeSpecifier(cp::TypeSpecifier::Int(
                            ct::Token::Int(0),
                        )),
                        cp::InitDeclaratorList::InitDeclarator(cp::InitDeclarator::Assign(
                            self.format_decl(name, ty),
                            ct::Token::Assign(0),
                            cp::AssignmentOrInitializerList::AssignmentExpression(
                                self.assignment_expression(expr),
                            ),
                        )),
                        ct::Token::Semicolon(0),
                    )))
                } else if let kp::Declaration::Declaration(public, mutable, name, Some(ty), None) =
                    decl
                {
                    cp::ExternalDeclaration::Declaration(Box::new(cp::Declaration::List(
                        cp::DeclarationSpecifiers::TypeSpecifier(cp::TypeSpecifier::Int(
                            ct::Token::Int(0),
                        )),
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

    pub fn assignment_expression(&self, k: kp::Assignment) -> cp::AssignmentExpression {
        match k {
            kp::Assignment::Expression(expr) => {
                cp::AssignmentExpression::TernaryExpression(self.expression(expr))
            }
        }
    }

    pub fn expression(&self, k: kp::Expression) -> cp::TernaryExpression {
        match k {
            kp::Expression::PrimaryExpression(primary_expr) => {
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

    pub fn primary_expr(&self, k: kp::PrimaryExpression) -> cp::PrimaryExpression {
        match k {
            kp::PrimaryExpression::Literal(kp::Literal::Integer(kt::Token::Integer(i))) => {
                cp::PrimaryExpression::Number(ct::Token::Number(0, i.to_string()))
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn format_decl(&self, name: Rc<str>, ty: kp::TypeSignature) -> cp::Declarator {
        if let kp::TypeSignature::Plain(_) = ty {
            cp::Declarator::Identifier(
                ct::Token::Identifier(0, name.to_string()),
                cp::DirectDeclarator::Epsilon(),
            )
        } else {
            panic!("Not implemented: {:?}", ty);
        }
    }
}
