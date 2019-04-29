use crate::traits::TreeTransformer;
use crate::Context;
/// Strip imports
use purkkasyntax::*;
use purkkaparser::visitor::*;

#[derive(Debug)]
pub struct StripImports<'a> {
    context: &'a mut Context,
}

impl<'a> TreeTransformer<'a> for StripImports<'a> {
    fn new(context: &'a mut Context) -> StripImports<'a> {
        StripImports { context }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for StripImports<'_> {
    fn visit_translation_unit(&mut self, e: &mut TranslationUnit) {
        match e {
            TranslationUnit::Units(ref mut units) => {
                units
                    .drain_filter(|t| {
                        if let Unit::ImportFile(_) = t {
                            true
                        } else {
                            false
                        }
                    })
                    .for_each(|t| {
                        match t {
                            Unit::ImportFile(ImportFile::Import(file, None)) => {
                                // XXX: global imports not implemented
                                self.context
                                    .local_includes
                                    .insert(From::from(format!("{}.h", file)));
                            }
                            otherwise => panic!("Not implemented: {:?}", otherwise),
                        }
                    });
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
