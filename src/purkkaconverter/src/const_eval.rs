/// Evaluate const expressions
use std::collections::HashMap;
use std::rc::Rc;

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
        EvalConstExprs { context, exprs: HashMap::new() }
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
            return Ok(())
        }
        match s {
            Declaration::Declaration(_, _, _, name, ty, assignment) =>
                vec![
                self.visit_ty(ty)?,
                self.fold_o(assignment, |v, a| {
                    let e = v.eval_expression(a)?;
                    v.exprs.insert(name.clone(), e);
                    Ok(())
                })?,
            ].flatten(self)
        }
    }
}

impl EvalConstExprs<'_> {
    fn eval_expression(&self, e: &mut Expression) -> Result<Literal, String> {
        let lit = match e {
            Expression::PrimaryExpression(p) => self.eval_primary_expression(p),
            Expression::Op(op, ExprList::List(args)) => match op.as_ref() {
                "+" => Ok(self.eval_expression(&mut args[0])? + self.eval_expression(&mut args[1])?),
                "*" => Ok(self.eval_expression(&mut args[0])? * self.eval_expression(&mut args[1])?),
                _ => unimplemented!("{}", op)
            }
            Expression::Unary(op, args) => unimplemented!(),
            Expression::PostFix(arg, op) => unimplemented!(),
            Expression::Cast(expr, ty) => unimplemented!(),
            Expression::Call(expr, args) => unimplemented!(),
            Expression::ArrayAccess(arr_expr, index_expr) => unimplemented!(),
            Expression::StructAccess(struct_expr, index) => unimplemented!(),
            Expression::Sizeof(sizeof_expr) => unimplemented!(),
        }?;
        *e = Expression::PrimaryExpression(PrimaryExpression::Literal(lit.clone()));
        Ok(lit)
    }

    fn eval_primary_expression(&self, e: &mut PrimaryExpression) -> Result<Literal, String> {
        match e {
            PrimaryExpression::Identifier(ident) => Ok(self.exprs[ident].clone()),
            PrimaryExpression::StructInitialization(struct_name, fields) => unimplemented!(),
            PrimaryExpression::VectorInitialization(vector_name, fields) => unimplemented!(),
            PrimaryExpression::ArrayLiteral(cells) => unimplemented!(),
            PrimaryExpression::Literal(lit) => Ok(lit.clone()),
            PrimaryExpression::BlockExpression(block) => unimplemented!(),
            PrimaryExpression::Expression(expr) => self.eval_expression(expr),
            PrimaryExpression::Lambda(lambda) => Err("Cannot evaluate lambdas without calling them".to_string()),
        }
    }
}
