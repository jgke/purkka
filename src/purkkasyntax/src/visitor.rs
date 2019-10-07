use std::ops::DerefMut;

use super::*;

pub trait ASTVisitor {
    type Succ;
    type Err;

    fn visit_s(&mut self, s: &mut S) -> Result<Self::Succ, Self::Err> {
        walk_s(self, s)
    }
    fn visit_translation_unit(&mut self, s: &mut TranslationUnit) -> Result<Self::Succ, Self::Err> {
        walk_translation_unit(self, s)
    }
    fn visit_unit(&mut self, s: &mut Unit) -> Result<Self::Succ, Self::Err> {
        walk_unit(self, s)
    }
    fn visit_import(&mut self, _s: &mut ImportFile) -> Result<Self::Succ, Self::Err> {
        Ok(self.ok())
    }
    fn visit_typedef(&mut self, s: &mut Typedef) -> Result<Self::Succ, Self::Err> {
        walk_typedef(self, s)
    }
    fn visit_operator_overload(
        &mut self,
        s: &mut OperatorOverload,
    ) -> Result<Self::Succ, Self::Err> {
        walk_operator_overload(self, s)
    }
    fn visit_declaration(&mut self, s: &mut Declaration) -> Result<Self::Succ, Self::Err> {
        walk_declaration(self, s)
    }
    fn visit_ty(&mut self, s: &mut TypeSignature) -> Result<Self::Succ, Self::Err> {
        walk_ty(self, s)
    }
    fn visit_intermediate_type(
        &mut self,
        s: &mut IntermediateType,
    ) -> Result<Self::Succ, Self::Err> {
        walk_intermediate_type(self, s)
    }
    fn visit_struct_field(&mut self, s: &mut StructField) -> Result<Self::Succ, Self::Err> {
        walk_struct_field(self, s)
    }
    fn visit_enum_field(&mut self, s: &mut EnumField) -> Result<Self::Succ, Self::Err> {
        walk_enum_field(self, s)
    }
    fn visit_expression(&mut self, s: &mut Expression) -> Result<Self::Succ, Self::Err> {
        walk_expression(self, s)
    }
    fn visit_primary_expression(
        &mut self,
        s: &mut PrimaryExpression,
    ) -> Result<Self::Succ, Self::Err> {
        walk_primary_expression(self, s)
    }
    fn visit_literal(&mut self, _s: &mut Literal) -> Result<Self::Succ, Self::Err> {
        Ok(self.ok())
    }
    fn visit_block_expression(&mut self, s: &mut BlockExpression) -> Result<Self::Succ, Self::Err> {
        walk_block_expression(self, s)
    }
    fn visit_block(&mut self, s: &mut Block) -> Result<Self::Succ, Self::Err> {
        walk_block(self, s)
    }
    fn visit_statement(&mut self, s: &mut Statement) -> Result<Self::Succ, Self::Err> {
        walk_statement(self, s)
    }
    fn visit_lambda(&mut self, s: &mut Lambda) -> Result<Self::Succ, Self::Err> {
        walk_lambda(self, s)
    }
    fn visit_param(&mut self, s: &mut Param) -> Result<Self::Succ, Self::Err> {
        walk_param(self, s)
    }
    fn visit_lambda_param(&mut self, s: &mut LambdaParam) -> Result<Self::Succ, Self::Err> {
        walk_lambda_param(self, s)
    }

    fn ok(&self) -> Self::Succ;
    fn concat(&self, left: Self::Succ, right: Self::Succ) -> Self::Succ;

    fn fold<F, T>(&mut self, list: &mut [T], mut map_fn: F) -> Result<Self::Succ, Self::Err>
    where
        F: FnMut(&mut Self, &mut T) -> Result<Self::Succ, Self::Err>,
    {
        list.iter_mut()
            .map(|f| map_fn(self, f))
            .collect::<Result<Vec<Self::Succ>, Self::Err>>()
            .map(|succs| {
                succs
                    .into_iter()
                    .fold(self.ok(), |left, right| self.concat(left, right))
            })
    }

    fn fold_o<F, T>(&mut self, list: &mut Option<T>, mut map_fn: F) -> Result<Self::Succ, Self::Err>
    where
        F: FnMut(&mut Self, &mut T) -> Result<Self::Succ, Self::Err>,
    {
        list.iter_mut()
            .map(|f| map_fn(self, f))
            .collect::<Result<Vec<Self::Succ>, Self::Err>>()
            .map(|succs| {
                succs
                    .into_iter()
                    .fold(self.ok(), |left, right| self.concat(left, right))
            })
    }

    fn fold_o_deref<F, T>(
        &mut self,
        list: &mut Option<Box<T>>,
        mut map_fn: F,
    ) -> Result<Self::Succ, Self::Err>
    where
        F: FnMut(&mut Self, &mut T) -> Result<Self::Succ, Self::Err>,
    {
        list.iter_mut()
            .map(|f| map_fn(self, f.deref_mut()))
            .collect::<Result<Vec<Self::Succ>, Self::Err>>()
            .map(|succs| {
                succs
                    .into_iter()
                    .fold(self.ok(), |left, right| self.concat(left, right))
            })
    }

    fn collect(&self, list: Vec<Self::Succ>) -> Result<Self::Succ, Self::Err> {
        Ok(list
            .into_iter()
            .fold(self.ok(), |left, right| self.concat(left, right)))
    }
}

pub fn walk_s<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut S) -> Result<T::Succ, T::Err> {
    match s {
        S::TranslationUnit(tu) => visitor.visit_translation_unit(tu),
    }
}

pub fn walk_translation_unit<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut TranslationUnit,
) -> Result<T::Succ, T::Err> {
    match s {
        TranslationUnit::Units(units) => visitor.fold(units, ASTVisitor::visit_unit),
    }
}

pub fn walk_unit<T: ASTVisitor + ?Sized>(visitor: &mut T, s: &mut Unit) -> Result<T::Succ, T::Err> {
    match s {
        Unit::Declaration(decl) => visitor.visit_declaration(decl),
        Unit::ImportFile(import) => visitor.visit_import(import),
        Unit::OperatorOverload(op) => visitor.visit_operator_overload(op),
        Unit::Typedef(ty) => visitor.visit_typedef(ty),
    }
}

pub fn walk_typedef<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut Typedef,
) -> Result<T::Succ, T::Err> {
    match s {
        Typedef::Alias(_, _, ty) => visitor.visit_ty(ty),
        Typedef::Struct(_, ref mut fields) => visitor.fold(fields, ASTVisitor::visit_struct_field),
        Typedef::Enum(_, ref mut fields) => visitor.fold(fields, ASTVisitor::visit_enum_field),
    }
}

pub fn walk_operator_overload<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut OperatorOverload,
) -> Result<T::Succ, T::Err> {
    match s {
        OperatorOverload::OperatorOverload(_, ty, body) => {
            vec![visitor.visit_ty(ty)?, visitor.visit_expression(body)?].flatten(visitor)
        }
    }
}

pub fn walk_declaration<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut Declaration,
) -> Result<T::Succ, T::Err> {
    match s {
        Declaration::Declaration(_flags, ty, decls) => vec![
            visitor.visit_ty(ty)?,
            visitor.fold(decls, |v, (_, e)|
                         v.fold_o(e, |v, e| v.visit_expression(e)))?
        ].flatten(visitor)
    }
}

pub fn walk_ty<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut TypeSignature,
) -> Result<T::Succ, T::Err> {
    match s {
        TypeSignature::Plain(..) => Ok(visitor.ok()),
        TypeSignature::Primitive(..) => Ok(visitor.ok()),
        TypeSignature::Vector(..) => Ok(visitor.ok()),
        TypeSignature::Complex(..) => Ok(visitor.ok()),
        TypeSignature::Pointer { ty, .. } => visitor.visit_ty(ty.deref_mut()),
        TypeSignature::Attribute(ty, _) => visitor.visit_ty(ty.deref_mut()),
        TypeSignature::Struct(_, ref mut fields) => {
            visitor.fold(fields, ASTVisitor::visit_struct_field)
        }
        TypeSignature::Enum(_, ref mut fields) => {
            visitor.fold(fields, ASTVisitor::visit_enum_field)
        }
        TypeSignature::Union(_, ref mut fields) => {
            visitor.fold(fields, ASTVisitor::visit_struct_field)
        }
        TypeSignature::Tuple(ref mut fields) => visitor.fold(fields, ASTVisitor::visit_ty),
        TypeSignature::Array(ref mut ty, _) => visitor.visit_ty(ty.deref_mut()),
        TypeSignature::DynamicArray(ref mut ty, ref mut expr) => vec![
            visitor.visit_ty(ty.deref_mut())?,
            visitor.visit_expression(expr.deref_mut())?,
        ]
        .flatten(visitor),
        TypeSignature::Function(ref mut params, ref mut return_type) => vec![
            visitor.fold(params, ASTVisitor::visit_param)?,
            visitor.visit_ty(return_type.deref_mut())?,
        ]
        .flatten(visitor),
        TypeSignature::Infer(ref mut intermediate) => visitor.visit_intermediate_type(intermediate),
    }
}

pub fn walk_intermediate_type<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut IntermediateType,
) -> Result<T::Succ, T::Err> {
    match s {
        IntermediateType::Exact(ref mut ty) => visitor.visit_ty(ty),
        IntermediateType::Any(_) => Ok(visitor.ok()),
        IntermediateType::Number(_, _) => Ok(visitor.ok()),
    }
}

pub fn walk_struct_field<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut StructField,
) -> Result<T::Succ, T::Err> {
    match s {
        StructField { ty, .. } => visitor.visit_ty(ty.deref_mut()),
    }
}

pub fn walk_enum_field<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut EnumField,
) -> Result<T::Succ, T::Err> {
    match s {
        EnumField { ty, .. } => visitor.fold_o(ty, ASTVisitor::visit_ty),
    }
}

pub fn walk_expression<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut Expression,
) -> Result<T::Succ, T::Err> {
    match s {
        Expression::PrimaryExpression(expr) => visitor.visit_primary_expression(expr),
        Expression::Op(_op, ExprList::List(exprs)) => {
            visitor.fold(exprs, ASTVisitor::visit_expression)
        }
        Expression::Unary(_op, ExprList::List(exprs)) => {
            visitor.fold(exprs, ASTVisitor::visit_expression)
        }
        Expression::PostFix(expr, _op) => visitor.visit_expression(expr),
        Expression::Cast(expr, ty) => {
            vec![visitor.visit_expression(expr)?, visitor.visit_ty(ty)?].flatten(visitor)
        }
        Expression::Call(expr, args) => vec![
            visitor.visit_expression(expr.deref_mut())?,
            visitor.fold(args, ASTVisitor::visit_expression)?,
        ]
        .flatten(visitor),
        Expression::ArrayAccess(array_expr, index_expr) => vec![
            visitor.visit_expression(array_expr.deref_mut())?,
            visitor.visit_expression(index_expr.deref_mut())?,
        ]
        .flatten(visitor),
        Expression::StructAccess(expr, _ident) => visitor.visit_expression(expr.deref_mut()),
        Expression::Sizeof(Sizeof::Expression(e)) => visitor.visit_expression(e),
        Expression::Sizeof(Sizeof::Type(t)) => visitor.visit_ty(t),
    }
}

pub fn walk_primary_expression<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut PrimaryExpression,
) -> Result<T::Succ, T::Err> {
    match s {
        PrimaryExpression::Identifier(_ident) => Ok(visitor.ok()),
        PrimaryExpression::Literal(literal) => visitor.visit_literal(literal),
        PrimaryExpression::BlockExpression(block) => {
            visitor.visit_block_expression(block.deref_mut())
        }
        PrimaryExpression::Expression(expr) => visitor.visit_expression(expr.deref_mut()),
        PrimaryExpression::Lambda(lambda) => visitor.visit_lambda(lambda),
        PrimaryExpression::StructInitialization(_ident, list) => visitor.fold(
            list,
            |v, StructInitializationField::StructInitializationField(_, e)| {
                v.visit_expression(e.deref_mut())
            },
        ),
        PrimaryExpression::VectorInitialization(_ident, list) => {
            visitor.fold(list, ASTVisitor::visit_expression)
        }
        PrimaryExpression::ArrayLiteral(list) => visitor.fold(list, ASTVisitor::visit_expression),
    }
}

pub fn walk_block_expression<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut BlockExpression,
) -> Result<T::Succ, T::Err> {
    match s {
        BlockExpression::Block(block) => visitor.visit_block(block),
        BlockExpression::If(arms, otherwise) => vec![
            visitor.fold(arms, |v, (condition, arm)| {
                v.visit_expression(condition.deref_mut())?;
                v.visit_block(arm.deref_mut())
            })?,
            visitor.fold_o_deref(otherwise, ASTVisitor::visit_block)?,
        ]
        .flatten(visitor),
        BlockExpression::While(expr, block, otherwise, _) => vec![
            visitor.visit_expression(expr.deref_mut())?,
            visitor.visit_block(block.deref_mut())?,
            visitor.fold_o_deref(otherwise, ASTVisitor::visit_block)?,
        ]
        .flatten(visitor),
        BlockExpression::For(init, cond, postloop, block, otherwise) => vec![
            visitor.fold_o_deref(init, ASTVisitor::visit_statement)?,
            visitor.fold_o_deref(cond, ASTVisitor::visit_expression)?,
            visitor.fold_o_deref(postloop, ASTVisitor::visit_expression)?,
            visitor.visit_block(block.deref_mut())?,
            visitor.fold_o_deref(otherwise, ASTVisitor::visit_block)?,
        ]
        .flatten(visitor),
    }
}

pub fn walk_block<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut Block,
) -> Result<T::Succ, T::Err> {
    match s {
        Block::Statements(statements) => visitor.fold(statements, ASTVisitor::visit_statement),
    }
}

pub fn walk_statement<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut Statement,
) -> Result<T::Succ, T::Err> {
    match s {
        Statement::Declaration(decl) => visitor.visit_declaration(decl),
        Statement::BlockExpression(block_expression) => {
            visitor.visit_block_expression(block_expression)
        }
        Statement::Expression(expression) => visitor.visit_expression(expression),
        Statement::Return(maybe_expression) => {
            visitor.fold_o_deref(maybe_expression, ASTVisitor::visit_expression)
        }
        Statement::Pragma(_pragma) => Ok(visitor.ok()),
        Statement::Jump(_jump) => Ok(visitor.ok()),
    }
}

pub fn walk_lambda<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut Lambda,
) -> Result<T::Succ, T::Err> {
    match s {
        Lambda::Lambda(params, ty, block) => vec![
            visitor.fold(params, ASTVisitor::visit_lambda_param)?,
            visitor.visit_ty(ty)?,
            visitor.visit_block(block)?,
        ]
        .flatten(visitor),
    }
}

pub fn walk_param<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut Param,
) -> Result<T::Succ, T::Err> {
    match s {
        Param::Param(_, ty) => visitor.visit_ty(ty),
        Param::TypeOnly(ty) => visitor.visit_ty(ty),
        Param::Variadic => Ok(visitor.ok()),
    }
}

pub fn walk_lambda_param<T: ASTVisitor + ?Sized>(
    visitor: &mut T,
    s: &mut LambdaParam,
) -> Result<T::Succ, T::Err> {
    match s {
        LambdaParam::LambdaParam(_, ty) => visitor.visit_ty(ty),
        LambdaParam::Variadic => Ok(visitor.ok()),
    }
}

pub trait Flatten<S, E, T: ?Sized> {
    fn flatten(self, visitor: &mut T) -> Result<S, E>;
}

impl<S, E, T> Flatten<S, E, T> for Vec<S>
where
    T: ASTVisitor<Succ = S, Err = E> + ?Sized,
{
    fn flatten(self, visitor: &mut T) -> Result<S, E> {
        visitor.collect(self)
    }
}

#[macro_export]
macro_rules! unit_result {
    () => {
        type Succ = ();
        fn ok(&self) -> () {
            ()
        }

        fn concat(&self, l: (), r: ()) -> () {
            ()
        }
    };
}
