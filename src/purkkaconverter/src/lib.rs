use std::collections::HashMap;
use std::rc::Rc;

use cparser::parser as cp;
use ctoken::token as ct;
use purkkaparser::{parser as pp, token as pt};

struct Context {
    functions: HashMap<Rc<str>, pp::Lambda>,
}

pub fn convert(purkka_tree: pp::S) -> cp::S {
    let mut context = Context {
        functions: HashMap::new(),
    };
    context.s(purkka_tree)
}

impl Context {
    fn push_function(&mut self, name: Rc<str>, lambda: pp::Lambda) {
        if self.functions.contains_key(&name) {
            panic!("Duplicate function: {}", name);
        }
        self.functions.insert(name, lambda);
    }

    fn push_anonymous_function(&mut self, name: Rc<str>, lambda: pp::Lambda) -> Rc<str> {
        let name: Rc<str> = From::from(format!("_lambda_{}", self.functions.len()));
        self.functions.insert(name.clone(), lambda);
        return name;
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
                u.into_iter()
                    .fold(cp::TranslationUnit::Epsilon(), |tree, next| {
                        cp::TranslationUnit::TranslationUnit(
                            Box::new(tree),
                            self.unit_to_external_decl(next),
                        )
                    })
            }
        }
    }

    pub fn unit_to_external_decl(&mut self, k: pp::Unit) -> cp::ExternalDeclaration {
        match k {
            pp::Unit::Declaration(decl) => {
                cp::ExternalDeclaration::Declaration(Box::new(self.convert_declaration(decl)))
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn convert_declaration(&mut self, k: pp::Declaration) -> cp::Declaration {
        match k {
            pp::Declaration::Declaration(false, _mutable, name, Some(ty), Some(expr)) => {
                cp::Declaration::List(
                    self.type_to_declaration_specifiers(ty.clone()),
                    cp::InitDeclaratorList::InitDeclarator(cp::InitDeclarator::Assign(
                        self.format_decl(name, ty),
                        ct::Token::Assign(0),
                        cp::AssignmentOrInitializerList::AssignmentExpression(
                            self.assignment_expression(expr),
                        ),
                    )),
                    ct::Token::Semicolon(0),
                )
            }
            pp::Declaration::Declaration(false, _mutable, name, Some(ty), None) => {
                cp::Declaration::List(
                    self.type_to_declaration_specifiers(ty.clone()),
                    cp::InitDeclaratorList::InitDeclarator(cp::InitDeclarator::Declarator(
                        self.format_decl(name, ty),
                    )),
                    ct::Token::Semicolon(0),
                )
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn type_to_declaration_specifiers(&mut self, ty: pp::TypeSignature) -> cp::DeclarationSpecifiers {
        match ty {
            pp::TypeSignature::Plain(ty) => {
                let c_ty = match ty.as_ref() {
                    "int" => cp::TypeSpecifier::Int(ct::Token::Int(0)),
                    other => panic!("Not implemented: {:?}", other),
                };
                cp::DeclarationSpecifiers::Neither(c_ty)
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn assignment_expression(&mut self, k: pp::Assignment) -> cp::AssignmentExpression {
        match k {
            pp::Assignment::Expression(expr) => {
                cp::AssignmentExpression::TernaryExpression(self.expression(expr))
            }
        }
    }

    pub fn expression(&mut self, k: pp::Expression) -> cp::TernaryExpression {
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

    pub fn primary_expr(&mut self, k: pp::PrimaryExpression) -> cp::PrimaryExpression {
        match k {
            pp::PrimaryExpression::Literal(pp::Literal::Integer(pt::Token::Integer(i))) => {
                cp::PrimaryExpression::Number(ct::Token::Number(0, i.to_string()))
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn format_decl(&mut self, name: Rc<str>, ty: pp::TypeSignature) -> cp::Declarator {
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
