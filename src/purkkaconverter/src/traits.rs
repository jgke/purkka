use crate::Context;
use purkkaparser::parser::S;

pub trait TreeTransformer<'a> {
    fn new(context: &'a mut Context) -> Self;
    fn transform(&mut self, s: &mut S);
}
