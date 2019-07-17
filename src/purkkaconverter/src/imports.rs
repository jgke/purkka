use crate::traits::TreeTransformer;
use crate::PurkkaToC;
use purkkasyntax::visitor::*;
/// Strip imports
use purkkasyntax::*;

#[derive(Debug)]
pub struct StripImports<'a> {
    context: &'a mut PurkkaToC,
}

#[allow(unused_must_use)]
impl<'a> TreeTransformer<'a> for StripImports<'a> {
    fn new(context: &'a mut PurkkaToC) -> StripImports<'a> {
        StripImports { context }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for StripImports<'_> {
    unit_result!();
    type Err = ();
    fn visit_translation_unit(&mut self, e: &mut TranslationUnit) -> Result<(), ()> {
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
                    .for_each(|t| match t {
                        Unit::ImportFile(box ImportFile::Import(file, None)) => {
                            if file.ends_with(".prk") {
                                self.context
                                    .local_includes
                                    .insert(From::from(format!("{}.h", &file[..file.len() - 4])));
                            } else {
                                self.context
                                    .local_includes
                                    .insert(From::from(format!("{}", file)));
                            }
                        }
                        otherwise => panic!("Not implemented: {:?}", otherwise),
                    });
            }
        }
        Ok(self.ok())
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
