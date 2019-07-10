/// Inline typedefs

use std::collections::HashMap;
use std::rc::Rc;

use crate::traits::TreeTransformer;
use crate::Context;
use purkkasyntax::visitor::*;
use purkkasyntax::*;

#[derive(Debug)]
pub struct InlineTypedef<'a> {
    context: &'a mut Context,
    typedefs: HashMap<Rc<str>, TypeSignature>
}

impl<'a> TreeTransformer<'a> for InlineTypedef<'a> {
    fn new(context: &'a mut Context) -> InlineTypedef<'a> {
        InlineTypedef { context, typedefs: HashMap::new() }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for InlineTypedef<'_> {
    fn visit_translation_unit(&mut self, e: &mut TranslationUnit) {
        match e {
            TranslationUnit::Units(ref mut units) => {
                units
                    .drain_filter(|t| {
                        if let Unit::Typedef(box Typedef::Alias(..)) = &*t {
                            true
                        } else {
                            false
                        }
                    })
                .for_each(|t| if let Unit::Typedef(box Typedef::Alias(_, name, ty)) = t {
                    self.typedefs.insert(name, *ty);
                } else {
                    unreachable!()
                });
            }
        }
        walk_translation_unit(self, e);
    }

    fn visit_ty(&mut self, e: &mut TypeSignature) {
        match e {
            TypeSignature::Plain(ident) if self.typedefs.contains_key(ident) => {
                *e = self.typedefs[ident].clone();
            }
            _ => {}
        }
        walk_ty(self, e);
    }
}
