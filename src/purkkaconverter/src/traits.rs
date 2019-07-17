use crate::PurkkaToC;
use purkkasyntax::S;

pub trait TreeTransformer<'a> {
    fn new(context: &'a mut PurkkaToC) -> Self;
    fn transform(&mut self, s: &mut S);
}
