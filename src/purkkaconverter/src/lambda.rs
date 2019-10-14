use crate::traits::TreeTransformer;
use crate::PurkkaToC;
use purkkasyntax::visitor::*;
/// Convert lambdas to global functions, replacing them in expressions with identifiers
use purkkasyntax::*;

#[derive(Debug)]
pub struct StripLambda<'a> {
    context: &'a mut PurkkaToC,
}

#[allow(unused_must_use)]
impl<'a> TreeTransformer<'a> for StripLambda<'a> {
    fn new(context: &'a mut PurkkaToC) -> StripLambda<'a> {
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
                        if let Unit::Declaration(box Declaration::Declaration(flags, _, decls)) = t {
                            for decl in decls {
                                if let (name, Some(box Expression::Lambda(
                                        mut lambda,
                                        ))) = decl {
                                    self.visit_lambda(&mut lambda).unwrap();
                                    self.context.push_function(name, lambda, flags);
                                } else {
                                    unreachable!()
                                }
                            }
                        } else {
                            unreachable!()
                        }
                    });
            }
        }
        walk_translation_unit(self, e)
    }

    fn visit_expression(&mut self, e: &mut Expression) -> Result<(), ()> {
        if let Expression::Lambda(_) = e {
            let mut ident_expr = Expression::Identifier(From::from(""));
            std::mem::swap(&mut ident_expr, e);
            if let Expression::Lambda(mut lambda) = ident_expr {
                let res = walk_lambda(self, &mut lambda);
                let name = self.context.push_anonymous_function(lambda);
                let mut actual_ident_expr = Expression::Identifier(name);
                std::mem::swap(&mut actual_ident_expr, e);
                res
            } else {
                unreachable!();
            }
        } else {
            walk_expression(self, e)
        }
    }
}
