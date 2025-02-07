/// Evaluate const expressions
use std::collections::HashMap;
use std::rc::Rc;
use std::ops::DerefMut;

use crate::traits::TreeTransformer;
use crate::PurkkaToC;
use purkkasyntax::visitor::*;
use purkkasyntax::*;

#[derive(Debug)]
pub struct EvalConstExprs<'a> {
    context: &'a mut PurkkaToC,
    exprs: HashMap<Rc<str>, Literal>,
}

#[allow(unused_must_use)]
impl<'a> TreeTransformer<'a> for EvalConstExprs<'a> {
    fn new(context: &'a mut PurkkaToC) -> EvalConstExprs<'a> {
        EvalConstExprs {
            context,
            exprs: HashMap::new(),
        }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for EvalConstExprs<'_> {
    unit_result!();
    type Err = String;

    // visit all top-level declarations without traversing the body
    fn visit_declaration(&mut self, s: &mut Declaration) -> Result<(), String> {
        if s.is_fn() {
            return Ok(());
        }
        match s {
            Declaration::Declaration(_flags, ty, decls) => vec![
                self.visit_ty(ty)?,
                self.fold(
                    decls,
                    |v, (name, e)|
                    v.fold_o(e, |v, a| {
                        if let Ok(e) = v.eval_expression(a) {
                            v.exprs.insert(name.clone(), e);
                        }
                        Ok(())
                    }))?
            ]
            .flatten(self),
        }
    }
}

impl EvalConstExprs<'_> {
    fn eval_expression(&self, e: &mut Expression) -> Result<Literal, String> {
        let lit = match e {
            Expression::Op(op, ExprList::List(args)) => match op.as_ref() {
                "+" => Ok(self.eval_expression(&mut args[0])? + self.eval_expression(&mut args[1])?),
                "-" => Ok(self.eval_expression(&mut args[0])? - self.eval_expression(&mut args[1])?),
                "*" => Ok(self.eval_expression(&mut args[0])? * self.eval_expression(&mut args[1])?),
                _ => unimplemented!("{}", op),
            },
            Expression::Unary(op, ExprList::List(args)) => match op.as_ref() {
                "+" => Ok(self.eval_expression(&mut args[0])?),
                "-" => Ok(-self.eval_expression(&mut args[0])?),
                _ => unimplemented!("{}", op),
            },
            Expression::PostFix(_arg, _op) => unimplemented!(),
            Expression::Cast(_expr, _ty) => unimplemented!(),
            Expression::Call(_expr, _args) => unimplemented!(),
            Expression::ArrayAccess(_arr_expr, _index_expr) => unimplemented!(),
            Expression::StructAccess(_struct_expr, _index) => unimplemented!(),
            Expression::Sizeof(_sizeof_expr) => unimplemented!(),

            Expression::Identifier(ident) => Ok(self.exprs[ident].clone()),
            Expression::StructInitialization(_struct_name, fields) => {
                for StructInitializationField::StructInitializationField(_, ref mut e) in fields.iter_mut() {
                    if let Ok(new_e) = self.eval_expression(e.deref_mut()) {
                        *e = Box::new(Expression::Literal(new_e));
                    }
                }
                Err("Cannot use structs as literals".to_string())
            }
            Expression::VectorInitialization(_vector_name, exprs) => {
                for ref mut e in exprs.iter_mut() {
                    if let Ok(new_e) = self.eval_expression(e.deref_mut()) {
                        **e = Expression::Literal(new_e);
                    }
                }
                Err("Not implemented".to_string())
            }
            Expression::ArrayLiteral(exprs) => {
                for ref mut e in exprs.iter_mut() {
                    if let Ok(new_e) = self.eval_expression(e.deref_mut()) {
                        **e = Expression::Literal(new_e);
                    }
                }
                Err("Not implemented".to_string())
            }
            Expression::Literal(lit) => Ok(lit.clone()),
            Expression::BlockExpression(_block) => unimplemented!(),
            Expression::Expression(expr) => self.eval_expression(expr),
            Expression::Lambda(_lambda) => {
                Err("Cannot evaluate lambdas without calling them".to_string())
            }
        }?;
        *e = Expression::Literal(lit.clone());
        Ok(lit)
    }
}
