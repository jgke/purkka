use std::ops::DerefMut;

use purkkatypes::{TypeSignature, EnumField, StructField, Param};

use crate::parser::*;

pub trait ASTVisitor {
    fn visit_s(&mut self, s: &mut S) { walk_s(self, s); }
    fn visit_translation_unit(&mut self, s: &mut TranslationUnit) { walk_translation_unit(self, s); }
    fn visit_unit(&mut self, s: &mut Unit) { walk_unit(self, s); }
    fn visit_import(&mut self, _s: &mut ImportFile) {}
    fn visit_declaration(&mut self, s: &mut Declaration) { walk_declaration(self, s); }
    fn visit_ty(&mut self, s: &mut TypeSignature) { walk_ty(self, s); }
    fn visit_struct_field(&mut self, s: &mut StructField) { walk_struct_field(self, s); }
    fn visit_enum_field(&mut self, s: &mut EnumField) { walk_enum_field(self, s); }
    fn visit_assignment(&mut self, s: &mut Assignment) { walk_assignment(self, s); }
    fn visit_expression(&mut self, s: &mut Expression) { walk_expression(self, s); }
    fn visit_primary_expression(&mut self, s: &mut PrimaryExpression) { walk_primary_expression(self, s); }
    fn visit_literal(&mut self, _s: &mut Literal) {}
    fn visit_block_expression(&mut self, s: &mut BlockExpression) { walk_block_expression(self, s); }
    fn visit_block(&mut self, s: &mut Block) { walk_block(self, s); }
    fn visit_conditional_expression(&mut self, s: &mut ConditionalExpression) {
        walk_conditional_expression(self, s);
    }
    fn visit_statement(&mut self, s: &mut Statement) { walk_statement(self, s); }
    fn visit_lambda(&mut self, s: &mut Lambda) { walk_lambda(self, s); }
    fn visit_param(&mut self, s: &mut Param) { walk_param(self, s); }
}

pub fn walk_s<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut S) {
    match s {
        S::TranslationUnit(tu) => visitor.visit_translation_unit(tu)
    }
}

pub fn walk_translation_unit<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut TranslationUnit) {
    match s {
        TranslationUnit::Units(units) => units.iter_mut().for_each(|i| visitor.visit_unit(i))
    }
}

pub fn walk_unit<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut Unit) {
    match s {
        Unit::Declaration(decl) => visitor.visit_declaration(decl),
        Unit::ImportFile(import) => visitor.visit_import(import),
        Unit::Typedef(_) => unimplemented!(),
    }
}

pub fn walk_declaration<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut Declaration) {
    match s {
        Declaration::Declaration(_, _, _, ty, assignment) => {
            if let Some(ty) = ty {
                visitor.visit_ty(ty);
            }
            if let Some(assignment) = assignment {
                visitor.visit_assignment(assignment);
            }
        }
    }
}

pub fn walk_ty<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut TypeSignature) {
    match s {
        TypeSignature::Plain(..) => {}
        TypeSignature::Pointer { ty, .. } => { visitor.visit_ty(ty.deref_mut()) }
        TypeSignature::Struct(_, ref mut fields)  => {
            fields.iter_mut().for_each(|ref mut f| visitor.visit_struct_field(f.deref_mut()))
        }
        TypeSignature::Enum(_, ref mut fields)  => {
            fields.iter_mut().for_each(|ref mut f| visitor.visit_enum_field(f.deref_mut()))
        }
        TypeSignature::Tuple(ref mut fields)  => {  fields.iter_mut().for_each(|f| visitor.visit_ty(f)) }
        TypeSignature::Array(ref mut ty, _)  => {
            visitor.visit_ty(ty.deref_mut());
        }
        TypeSignature::Function(ref mut params, ref mut return_type)  => {
            params.iter_mut().for_each(|ref mut p| visitor.visit_param(p));
            visitor.visit_ty(return_type.deref_mut());
        }
        TypeSignature::Infer  => {}
    }
}

pub fn walk_struct_field<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut StructField) {
    match s {
        StructField::Field { ty, .. } => visitor.visit_ty(ty.deref_mut())
    }
}

pub fn walk_enum_field<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut EnumField) {
    match s {
        EnumField::Field { ty, .. } => {
            ty.iter_mut().for_each(|ty| visitor.visit_ty(ty));
        }
    }
}

pub fn walk_assignment<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut Assignment) {
    match s {
        Assignment::Expression(expr) => {
            visitor.visit_expression(expr);
        }
    }
}

pub fn walk_expression<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut Expression) {
    match s {
        Expression::PrimaryExpression(expr) => {
            visitor.visit_primary_expression(expr);
        }
        Expression::Op(_op, ExprList::List(exprs)) => {
            exprs.iter_mut().for_each(|e| visitor.visit_expression(e));
        }
        Expression::Unary(_op, ExprList::List(exprs)) => {
            exprs.iter_mut().for_each(|e| visitor.visit_expression(e));
        }
        Expression::Prefix(_op, expr) => {
            visitor.visit_expression(expr);
        }
    }
}

pub fn walk_primary_expression<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut PrimaryExpression) {
    match s {
        PrimaryExpression::Identifier(_ident) => {}
        PrimaryExpression::Call(_, ArgList::Args(ref mut args)) => {
            args.iter_mut().for_each(|ref mut a| visitor.visit_expression(a.deref_mut()));
        }
        PrimaryExpression::Literal(literal) => {
            visitor.visit_literal(literal);
        }
        PrimaryExpression::BlockExpression(block) => {
            visitor.visit_block_expression(block.deref_mut());
        }
        PrimaryExpression::Expression(expr) => {
            visitor.visit_expression(expr.deref_mut());
        }
        PrimaryExpression::Lambda(lambda) => {
            visitor.visit_lambda(lambda);
        }
    }
}

pub fn walk_block_expression<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut BlockExpression) {
    match s {
        BlockExpression::Block(block) => {
            visitor.visit_block(block);
        }
        BlockExpression::ConditionalExpression(cond) => {
            visitor.visit_conditional_expression(cond)
        }
    }
}

pub fn walk_conditional_expression<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut ConditionalExpression) {
    match s {
        ConditionalExpression::Exprs(arms, otherwise) => {
            arms.iter_mut().for_each(|(condition, arm)| {
                visitor.visit_expression(condition.deref_mut());
                visitor.visit_block(arm.deref_mut());
            });
            otherwise.iter_mut().for_each(|b| visitor.visit_block(b.deref_mut()));
        }
    }
}

pub fn walk_block<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut Block) {
    match s {
        Block::Statements(statements) => {
            statements.iter_mut().for_each(|statement| visitor.visit_statement(statement));
        }
    }
}

pub fn walk_statement<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut Statement) {
    match s {
        Statement::Declaration(decl) => {
            visitor.visit_declaration(decl);
        }
        Statement::BlockExpression(block_expression) => {
            visitor.visit_block_expression(block_expression);
        }
        Statement::Expression(expression) => {
            visitor.visit_expression(expression);
        }
        Statement::Return(maybe_expression) => {
            maybe_expression.iter_mut().for_each(|b| visitor.visit_expression(b));
        }
    }
}

pub fn walk_lambda<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut Lambda) {
    match s {
        Lambda::Lambda(params, ty, block_expr) => {
            params.iter_mut().for_each(|p| visitor.visit_param(p));
            visitor.visit_ty(ty);
            visitor.visit_block_expression(block_expr);
        }
    }
}

pub fn walk_param<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut Param) {
    match s {
        Param::Param(_, ty) => {
            visitor.visit_ty(ty);
        }
        Param::Anon(ty) => {
            visitor.visit_ty(ty);
        }
    }
}
