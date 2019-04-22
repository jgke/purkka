/// Convert [T] to *T

use purkkaparser::parser::*;
use purkkaparser::token::Token::Identifier;
use purkkaparser::visitor::*;
use crate::traits::TreeTransformer;
use crate::Context;

#[derive(Debug)]
pub struct ArrayToPointer<'a> {
    context: &'a mut Context,
}

impl<'a> TreeTransformer<'a> for ArrayToPointer<'a> {
    fn new(context: &'a mut Context) -> ArrayToPointer<'a> {
        ArrayToPointer { context }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for ArrayToPointer<'_> {
    fn visit_ty(&mut self, e: &mut TypeSignature) {
        dbg!(&e);
        if let TypeSignature::Array(ty, None) = e {
            let mut tmp = TypeSignature::Infer;
            std::mem::swap(&mut tmp, e);
            if let TypeSignature::Array(mut ty, None) = tmp {
                self.visit_ty(&mut ty);
                *e  = TypeSignature::Pointer { nullable: false, ty } ;
            } else {
                unreachable!()
            }
        } else {
            walk_ty(self, e);
        }
    }
}
