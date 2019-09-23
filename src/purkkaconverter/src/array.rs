/// Convert [T] to *T in parameters
use crate::traits::TreeTransformer;
use crate::PurkkaToC;
use purkkasyntax::visitor::*;
use purkkasyntax::*;

#[derive(Debug)]
pub struct ArrayToPointer<'a> {
    context: &'a mut PurkkaToC,
    inside_param: bool,
}

#[allow(unused_must_use)]
impl<'a> TreeTransformer<'a> for ArrayToPointer<'a> {
    fn new(context: &'a mut PurkkaToC) -> ArrayToPointer<'a> {
        ArrayToPointer {
            context,
            inside_param: false,
        }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
        let mut context = PurkkaToC::default();
        for decl in &mut self.context.symbols.declarations.0 {
            ArrayToPointer {
                context: &mut context,
                inside_param: false,
            }
            .visit_ty(decl.1);
        }
        for decl in &mut self.context.symbols.imported_declarations.0 {
            ArrayToPointer {
                context: &mut context,
                inside_param: false,
            }
            .visit_ty(decl.1);
        }
    }
}

impl ASTVisitor for ArrayToPointer<'_> {
    unit_result!();
    type Err = ();

    fn visit_lambda_param(&mut self, e: &mut LambdaParam) -> Result<(), ()> {
        match e {
            LambdaParam::Variadic => Ok(()),
            LambdaParam::LambdaParam(_, ty) => {
                self.inside_param = true;
                let res = self.visit_ty(&mut **ty);
                self.inside_param = false;
                res
            }
        }
    }

    fn visit_param(&mut self, e: &mut Param) -> Result<(), ()> {
        match e {
            Param::Param(_, ty) | Param::TypeOnly(ty) => {
                self.inside_param = true;
                let res = self.visit_ty(&mut **ty);
                self.inside_param = false;
                res
            }
            Param::Variadic => Ok(()),
        }
    }

    fn visit_ty(&mut self, e: &mut TypeSignature) -> Result<(), ()> {
        if self.inside_param {
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
        } else {
            walk_ty(self, e)
        }
    }
}
