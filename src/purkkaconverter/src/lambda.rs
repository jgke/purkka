use crate::traits::TreeTransformer;
use crate::Context;
/// Convert lambdas to global functions, replacing them in expressions with identifiers
use purkkaparser::parser::*;
use purkkaparser::visitor::*;

#[derive(Debug)]
pub struct StripLambda<'a> {
    context: &'a mut Context,
}

impl<'a> TreeTransformer<'a> for StripLambda<'a> {
    fn new(context: &'a mut Context) -> StripLambda<'a> {
        StripLambda { context }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for StripLambda<'_> {
    fn visit_translation_unit(&mut self, e: &mut TranslationUnit) {
        match e {
            TranslationUnit::Units(ref mut units) => {
                units
                    .drain_filter(|t| {
                        if let Unit::Declaration(decl) = t {
                            decl.is_fn()
                        } else {
                            false
                        }
                    })
                    .for_each(|t| {
                        if let Unit::Declaration(Declaration::Declaration(
                            _,
                            _,
                            name,
                            _,
                            Some(Expression::PrimaryExpression(
                                PrimaryExpression::Lambda(mut lambda),
                            )),
                        )) = t
                        {
                            self.visit_lambda(&mut lambda);
                            self.context.push_function(name, lambda);
                        } else {
                            unreachable!()
                        }
                    });
                units.iter_mut().for_each(|u| self.visit_unit(u));
            }
        }
    }

    fn visit_primary_expression(&mut self, e: &mut PrimaryExpression) {
        if let PrimaryExpression::Lambda(_) = e {
            let mut ident_expr = PrimaryExpression::Identifier(From::from(""));
            std::mem::swap(&mut ident_expr, e);
            if let PrimaryExpression::Lambda(mut lambda) = ident_expr {
                walk_lambda(self, &mut lambda);
                let name = self.context.push_anonymous_function(lambda);
                let mut actual_ident_expr = PrimaryExpression::Identifier(name);
                std::mem::swap(&mut actual_ident_expr, e);
            } else {
                unreachable!();
            }
        } else {
            walk_primary_expression(self, e);
        }
    }
}
