use crate::traits::TreeTransformer;
use crate::Context;
use purkkasyntax::visitor::*;
/// Convert lambdas to global functions, replacing them in expressions with identifiers
use purkkasyntax::*;

#[derive(Debug)]
pub struct StripLambda<'a> {
    context: &'a mut Context,
}

#[allow(unused_must_use)]
impl<'a> TreeTransformer<'a> for StripLambda<'a> {
    fn new(context: &'a mut Context) -> StripLambda<'a> {
        StripLambda { context }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s).unwrap();
    }
}

impl ASTVisitor for StripLambda<'_> {
    unit_result!();
    type Err = ();
    fn visit_translation_unit(&mut self, e: &mut TranslationUnit) -> Result<(), ()> {
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
                        if let Unit::Declaration(box Declaration::Declaration(
                            _,
                            _,
                            inline,
                            name,
                            _,
                            Some(box Expression::PrimaryExpression(PrimaryExpression::Lambda(
                                mut lambda,
                            ))),
                        )) = t
                        {
                            self.visit_lambda(&mut lambda).unwrap();
                            self.context.push_function(name, lambda, inline);
                        } else {
                            unreachable!()
                        }
                    });
            }
        }
        walk_translation_unit(self, e)
    }

    fn visit_primary_expression(&mut self, e: &mut PrimaryExpression) -> Result<(), ()> {
        if let PrimaryExpression::Lambda(_) = e {
            let mut ident_expr = PrimaryExpression::Identifier(From::from(""));
            std::mem::swap(&mut ident_expr, e);
            if let PrimaryExpression::Lambda(mut lambda) = ident_expr {
                let res = walk_lambda(self, &mut lambda);
                let name = self.context.push_anonymous_function(lambda);
                let mut actual_ident_expr = PrimaryExpression::Identifier(name);
                std::mem::swap(&mut actual_ident_expr, e);
                res
            } else {
                unreachable!();
            }
        } else {
            walk_primary_expression(self, e)
        }
    }
}
