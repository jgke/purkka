use crate::traits::TreeTransformer;
use crate::Context;
use purkkasyntax::visitor::*;
/// Convert [T] to *T
use purkkasyntax::*;

#[derive(Debug)]
pub struct ArrayToPointer<'a> {
    context: &'a mut Context,
}

#[allow(unused_must_use)]
impl<'a> TreeTransformer<'a> for ArrayToPointer<'a> {
    fn new(context: &'a mut Context) -> ArrayToPointer<'a> {
        ArrayToPointer { context }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for ArrayToPointer<'_> {
    unit_result!();
    type Err = ();

    fn visit_ty(&mut self, e: &mut TypeSignature) -> Result<(), ()> {
        if let TypeSignature::Array(_, None) = e {
            let mut tmp = TypeSignature::Infer(IntermediateType::new_any());
            std::mem::swap(&mut tmp, e);
            if let TypeSignature::Array(mut ty, None) = tmp {
                let res = self.visit_ty(&mut ty);
                *e = TypeSignature::Pointer {
                    nullable: false,
                    ty,
                };
                res
            } else {
                unreachable!()
            }
        } else {
            walk_ty(self, e)
        }
    }
}
