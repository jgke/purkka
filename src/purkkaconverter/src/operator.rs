/// Inline overrided operators
use crate::traits::TreeTransformer;
use crate::PurkkaToC;
use purkkasyntax::visitor::*;
use purkkasyntax::*;

#[derive(Debug)]
pub struct InlineOperators<'a> {
    context: &'a mut PurkkaToC,
}

#[allow(unused_must_use)]
impl<'a> TreeTransformer<'a> for InlineOperators<'a> {
    fn new(context: &'a mut PurkkaToC) -> InlineOperators<'a> {
        InlineOperators { context }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for InlineOperators<'_> {
    unit_result!();
    type Err = ();
    fn visit_translation_unit(&mut self, e: &mut TranslationUnit) -> Result<(), ()> {
        match e {
            TranslationUnit::Units(ref mut units) => {
                units
                    .drain_filter(|t| {
                        if let Unit::OperatorOverload(_) = t {
                            true
                        } else {
                            false
                        }
                    })
                    .last();
            }
        }
        walk_translation_unit(self, e)
    }

    fn visit_expression(&mut self, e: &mut Expression) -> Result<(), ()> {
        if let Expression::Op(op, list) = e {
            let ExprList::List(list) = list;
            let operator = &self.context.operators.infix[op];
            if let Some(expr) = &operator.handler {
                *e = Expression::Call(Box::new(expr.clone()), ArgList::Args(list.clone()))
            }
        }
        walk_expression(self, e)
    }
}
