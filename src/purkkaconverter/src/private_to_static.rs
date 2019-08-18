/// Make all private global declarations static
use crate::traits::TreeTransformer;
use crate::PurkkaToC;
use purkkasyntax::visitor::*;
use purkkasyntax::*;

#[derive(Debug)]
pub struct PrivateToStatic<'a> {
    context: &'a mut PurkkaToC,
}

#[allow(unused_must_use)]
impl<'a> TreeTransformer<'a> for PrivateToStatic<'a> {
    fn new(context: &'a mut PurkkaToC) -> PrivateToStatic<'a> {
        PrivateToStatic { context }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for PrivateToStatic<'_> {
    unit_result!();
    type Err = String;

    // visit all top-level declarations without traversing the body
    fn visit_declaration(&mut self, s: &mut Declaration) -> Result<(), String> {
        let Declaration::Declaration(flags, ..) = s;
        if !flags.public {
            flags.static_ = true;
        }
        Ok(())
    }
}
