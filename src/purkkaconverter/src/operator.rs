/// Inline overrided operators

use crate::traits::TreeTransformer;
use crate::Context;
use purkkasyntax::visitor::*;
use purkkasyntax::*;

#[derive(Debug)]
pub struct InlineOperators<'a> {
    context: &'a mut Context,
}

impl<'a> TreeTransformer<'a> for InlineOperators<'a> {
    fn new(context: &'a mut Context) -> InlineOperators<'a> {
        InlineOperators { context }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for InlineOperators<'_> {
    fn visit_translation_unit(&mut self, e: &mut TranslationUnit) {
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
        walk_translation_unit(self, e);
    }

    fn visit_expression(&mut self, e: &mut Expression) {
        if let Expression::Op(op, list) = e {
            let ExprList::List(list) = list;
            let operator = &self.context.operators.infix[op];
            if let Some(expr) = &operator.handler {
                *e = Expression::Call(Box::new(expr.clone()), ArgList::Args(list.clone()))
            }
        }
        walk_expression(self, e);
    }
}
