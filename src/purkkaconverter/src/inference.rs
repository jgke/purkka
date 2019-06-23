/// Infer types
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

use crate::traits::TreeTransformer;
use crate::Context;
use purkkaparser::visitor::*;
use purkkasyntax::*;
use purkkatoken::token::Token;

#[derive(Debug)]
pub struct TypeInferrer {
    scope: Vec<HashMap<Rc<str>, IntermediateType>>,
    infer_map: HashMap<i128, IntermediateType>,
    ty_index: i128,
}

fn get_std_lib() -> HashMap<Rc<str>, IntermediateType> {
    let mut map = HashMap::new();

    fn param(ty: &TypeSignature) -> Param {
        Param::TypeOnly(Box::new(ty.clone()))
    }

    let void = TypeSignature::Plain(From::from("void"));
    let ulong = TypeSignature::Plain(From::from("ulong"));
    let int = TypeSignature::Plain(From::from("int"));
    let void_ptr = TypeSignature::Pointer {
        ty: Box::new(void),
        nullable: true,
    };

    map.insert(
        From::from("malloc"),
        IntermediateType::Exact(Box::new(TypeSignature::Function(
            vec![param(&ulong)],
            Box::new(void_ptr.clone()),
        ))),
    );
    map.insert(
        From::from("write"),
        IntermediateType::Exact(Box::new(TypeSignature::Function(
            vec![param(&int), param(&void_ptr), param(&ulong)],
            Box::new(ulong.clone()),
        ))),
    );

    map
}

impl<'a> TreeTransformer<'a> for TypeInferrer {
    fn new(_context: &'a mut Context) -> TypeInferrer {
        TypeInferrer {
            scope: vec![get_std_lib()],
            infer_map: HashMap::new(),
            ty_index: -1,
        }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);

        let mut ti = TypeInserter {
            infer_map: &mut self.infer_map,
        };
        ti.visit_s(s);
    }
}

impl ASTVisitor for TypeInferrer {
    fn visit_declaration(&mut self, tree: &mut Declaration) {
        match tree {
            Declaration::Declaration(_, _, name, exact_ty, Some(e)) => {
                let ty = self.get_type(e);
                self.push_type(name.clone(), ty.0.clone());
                let intermediate: IntermediateType = From::from(*exact_ty.clone());
                if !ty.1.is_empty() {
                    match &**exact_ty {
                        TypeSignature::Function(_, ret) => {
                            for ret_ty in ty.1 {
                                self.make_equal(&ret_ty, &From::from(*ret.clone()));
                            }
                        }
                        otherwise => panic!("Not implemented: {:?}", otherwise),
                    }
                }
                self.make_equal(&ty.0, &intermediate);
                self.push_type(name.clone(), intermediate);
            }
            Declaration::Declaration(_, _, name, ty, None) => {
                self.push_type(name.clone(), From::from(*ty.clone()));
            }
        }
        walk_declaration(self, tree);
    }

    fn visit_lambda(&mut self, tree: &mut Lambda) {
        self.push_block();
        let Lambda::Lambda(params, _, e) = tree;
        for LambdaParam::LambdaParam(name, ty) in params {
            self.push_type(name.clone(), From::from(*ty.clone()));
        }
        walk_block_expression(self, e);
        self.pop_block();
    }

    fn visit_block(&mut self, e: &mut Block) {
        self.push_block();
        walk_block(self, e);
        self.pop_block();
    }

    fn visit_primary_expression(&mut self, e: &mut PrimaryExpression) {
        self.push_block();
        if let PrimaryExpression::Lambda(Lambda::Lambda(params, _, _)) = e {
            for LambdaParam::LambdaParam(name, ty) in params {
                self.push_type(name.clone(), From::from(*ty.clone()));
            }
        }
        walk_primary_expression(self, e);
        self.pop_block();
    }
}

impl TypeInferrer {
    fn push_block(&mut self) {
        self.scope.push(HashMap::new());
    }

    fn push_type(&mut self, name: Rc<str>, ty: IntermediateType) {
        self.scope.last_mut().unwrap().insert(name, ty);
    }

    fn pop_block(&mut self) {
        self.scope.pop();
    }

    fn get_ident_ty(&self, t: &str) -> IntermediateType {
        for tys in self.scope.iter().rev() {
            if let Some(ty) = tys.get(t) {
                return ty.clone();
            }
        }
        panic!("Unknown identifier: {}", t);
    }

    fn get_ty_index(&mut self) -> i128 {
        self.ty_index -= 1;
        self.ty_index
    }

    fn get_any_ty(&mut self) -> IntermediateType {
        IntermediateType::Any(self.get_ty_index())
    }

    fn get_number_ty(&mut self) -> IntermediateType {
        IntermediateType::Number(self.get_ty_index(), IntermediateNumber::Indeterminate)
    }

    fn get_type(&mut self, expression: &Expression) -> (IntermediateType, Vec<IntermediateType>) {
        match expression {
            Expression::PrimaryExpression(expr) => self.get_primary_expr_type(expr),
            Expression::Op(t, ExprList::List(list)) => match t.as_ref() {
                "+" | "-" | "*" | "%" => {
                    assert_eq!(list.len(), 2);
                    let left = self.get_type(&list[0]);
                    let right = self.get_type(&list[1]);
                    let num = self.get_number_ty();
                    self.make_equal(&left.0, &num);
                    self.make_equal(&left.0, &right.0);
                    (
                        left.0,
                        left.1.into_iter().chain(right.1.into_iter()).collect(),
                    )
                }
                "&" => {
                    assert_eq!(list.len(), 1);
                    let ty = self.get_type(&list[0]);

                    (
                        IntermediateType::Exact(Box::new(TypeSignature::Pointer {
                            ty: Box::new(From::from(ty.0)),
                            nullable: false,
                        })),
                        ty.1,
                    )
                }
                "=" => {
                    assert_eq!(list.len(), 2);
                    let left = self.get_type(&list[0]);
                    let right = self.get_type(&list[1]);
                    self.make_equal(&left.0, &right.0);
                    (
                        left.0,
                        left.1.into_iter().chain(right.1.into_iter()).collect(),
                    )
                }
                "<" | ">" | "<=" | ">=" => {
                    assert_eq!(list.len(), 2);
                    let num_ty = self.get_number_ty();
                    let left = self.get_type(&list[0]);
                    let right = self.get_type(&list[1]);
                    self.make_equal(&left.0, &right.0);
                    (
                        num_ty,
                        left.1.into_iter().chain(right.1.into_iter()).collect(),
                    )
                }
                otherwise => panic!("Not implemented: {:?}", otherwise),
            },
            Expression::PostFix(e, t) => match t.as_ref() {
                "++" => {
                    let ty = self.get_type(&e);
                    self.make_equal_to_num(&ty.0);
                    ty
                }
                otherwise => panic!("Not implemented: {:?}", otherwise),
            },
            Expression::ArrayAccess(expr, index) => self.get_array_access_type(expr, index),
            Expression::Call(expr, ArgList::Args(args)) => {
                let (ty, mut ret_tys) = self.get_type(&*expr);
                for arg in args {
                    let (_, mut more_ret_tys) = self.get_type(arg);
                    ret_tys.append(&mut more_ret_tys);
                }

                match From::from(ty) {
                    t @ TypeSignature::Function(..) => (self.call_exact(&t, args), ret_tys),
                    TypeSignature::Infer(inferred) => {
                        (self.call_inferred(&inferred, args), ret_tys)
                    }
                    otherwise => panic!("Not implemented: {:?}", otherwise),
                }
            }
            Expression::Cast(expr, ty) => (From::from(ty.clone()), self.get_type(expr).1),
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }

    fn get_primary_expr_type(
        &mut self,
        expr: &PrimaryExpression,
    ) -> (IntermediateType, Vec<IntermediateType>) {
        match expr {
            PrimaryExpression::Identifier(t) => (self.get_ident_ty(t.as_ref()), Vec::new()),
            PrimaryExpression::Literal(Literal::Integer(..)) => (self.get_number_ty(), Vec::new()),
            PrimaryExpression::Literal(Literal::Float(..)) => (IntermediateType::Number(self.get_ty_index(), IntermediateNumber::Float), Vec::new()),
            PrimaryExpression::Lambda(Lambda::Lambda(params, return_type, block)) => {
                if let TypeSignature::Infer(_id) = return_type {
                    self.push_block();
                    for LambdaParam::LambdaParam(name, ty) in params {
                        self.push_type(name.clone(), From::from(*ty.clone()));
                    }
                    let block_ty = self.get_block_expr_type(&*block);
                    let ret_tys = block_ty.1;
                    if let Some((last, rest)) = ret_tys.split_last() {
                        for ty in rest {
                            self.make_equal(&ty, &last);
                        }
                    }
                    let ret_ty = if let Some(ty) = block_ty.0 {
                        if let Some(ret_ty) = ret_tys.get(0) {
                            self.make_equal(&ty, &ret_ty);
                        }
                        ty
                    } else if let Some(ty) = ret_tys.get(0) {
                        ty.clone()
                    } else {
                        From::from(TypeSignature::Plain(From::from("void")))
                    };
                    self.make_equal(&From::from(return_type.clone()), &ret_ty);
                    let ty = From::from(TypeSignature::Function(
                        params.iter().cloned().map(From::from).collect(),
                        Box::new(From::from(ret_ty)),
                    ));
                    self.pop_block();
                    (ty, Vec::new())
                } else {
                    (
                        From::from(TypeSignature::Function(
                            params.iter().cloned().map(From::from).collect(),
                            Box::new(return_type.clone()),
                        )),
                        Vec::new(),
                    )
                }
            }
            PrimaryExpression::Expression(expr) => self.get_type(expr),
            PrimaryExpression::StructInitialization(ident, _) => {
                (From::from(TypeSignature::Plain(ident.clone())), Vec::new())
            }
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }

    fn get_array_access_type(
        &mut self,
        array_expr: &Expression,
        index_expr: &Expression,
    ) -> (IntermediateType, Vec<IntermediateType>) {
        let (ty, mut ret_tys) = self.get_type(array_expr);
        let (index_ty, index_ret_tys) = self.get_type(index_expr);
        ret_tys.extend(index_ret_tys);
        self.make_equal_to_num(&index_ty);
        (self.array_access_inferred(&ty, index_expr), ret_tys)
    }

    fn array_access_exact(
        &mut self,
        array_ty: &TypeSignature,
        index_expr: &Expression,
    ) -> IntermediateType {
        match array_ty {
            TypeSignature::Tuple(list) => {
                let lit = index_expr
                    .eval(&HashMap::new())
                    .expect("Tuple index must be a constant expression");
                let index = match lit {
                    Literal::Integer(Token::Integer(_, i)) => i,
                    otherwise => panic!(
                        "Tuple index must be a integer constant expression (got {:?})",
                        otherwise
                    ),
                };
                From::from(list[usize::try_from(index).unwrap()].clone())
            }
            TypeSignature::Pointer { ty, .. }
            | TypeSignature::Array(ty, _)
            | TypeSignature::DynamicArray(ty, _) => From::from(*ty.clone()),
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }

    fn array_access_inferred(
        &mut self,
        inferred: &IntermediateType,
        index_expr: &Expression,
    ) -> IntermediateType {
        match inferred {
            IntermediateType::Exact(left) => self.array_access_exact(left, index_expr),
            IntermediateType::Number(..) => panic!("Cannot index numbers"),
            IntermediateType::Any(id) => {
                if let Some(ty) = self.infer_map.get(id).cloned() {
                    self.array_access_inferred(&ty, index_expr)
                } else {
                    let any = self.get_any_ty();
                    let arr_ty = IntermediateType::Exact(Box::new(TypeSignature::Array(
                        Box::new(From::from(any)),
                        None,
                    )));
                    self.infer_map.insert(*id, arr_ty.clone());
                    arr_ty
                }
            }
        }
    }

    fn get_block_expr_type(
        &mut self,
        expr: &BlockExpression,
    ) -> (Option<IntermediateType>, Vec<IntermediateType>) {
        match expr {
            BlockExpression::Block(block) => self.get_block_type(&*block),
            BlockExpression::If(arms, otherwise) => {
                if let Some(otherwise) = otherwise {
                    arms.iter()
                        .map(|(expr, b)| {
                            let num_ty = self.get_number_ty();
                            let expr_ty = self.get_type(expr);
                            self.make_equal(&num_ty, &expr_ty.0);
                            self.get_block_type(b)
                        })
                        .collect::<Vec<_>>()
                        .iter()
                        .fold(
                            self.get_block_type(otherwise),
                            |(prev, mut ret_vals), (this, more_ret_vals)| {
                                if let Some(t) = prev.as_ref() {
                                    self.make_equal(t, this.as_ref().unwrap())
                                }
                                ret_vals.append(&mut more_ret_vals.clone());
                                (prev, ret_vals)
                            },
                        )
                } else {
                    (None, Vec::new())
                }
            }
            BlockExpression::While(e, block, else_block) => {
                let (ty, mut ret_tys) = self.get_type(&*e);
                let num = self.get_number_ty();
                self.make_equal(&ty, &num);

                let (_, more_ret_tys) = self.get_block_type(block);
                let even_more_ret_tys = else_block
                    .as_ref()
                    .map(|b| self.get_block_type(b).1)
                    .unwrap_or_else(Vec::new);
                ret_tys.extend(more_ret_tys);
                ret_tys.extend(even_more_ret_tys);
                (None, ret_tys)
            }
            BlockExpression::For(s1, s2, s3, block, else_block) => {
                self.push_block();
                let num = self.get_number_ty();

                let mut ret_tys = s1
                    .as_ref()
                    .map(|s| self.get_statement_type(&*s).1)
                    .unwrap_or_else(Vec::new);
                let (ty, ret_tys_2) = s2
                    .as_ref()
                    .map(|s| self.get_type(&*s))
                    .unwrap_or_else(|| (num.clone(), Vec::new()));
                self.make_equal(&ty, &num);
                let ret_tys_3 = s3
                    .as_ref()
                    .map(|s| self.get_type(&*s).1)
                    .unwrap_or_else(Vec::new);

                let (_, ret_tys_4) = self.get_block_type(block);
                let ret_tys_5 = else_block
                    .as_ref()
                    .map(|b| self.get_block_type(b).1)
                    .unwrap_or_else(Vec::new);

                ret_tys.extend(ret_tys_2);
                ret_tys.extend(ret_tys_3);
                ret_tys.extend(ret_tys_4);
                ret_tys.extend(ret_tys_5);

                self.pop_block();

                (None, ret_tys)
            }
        }
    }

    fn get_block_type(
        &mut self,
        expr: &Block,
    ) -> (Option<IntermediateType>, Vec<IntermediateType>) {
        match expr {
            Block::Statements(stmts) => stmts.iter().map(|e| self.get_statement_type(e)).fold(
                (None, Vec::new()),
                |(_, mut prev_list), (cur, mut cur_list)| {
                    prev_list.append(&mut cur_list);
                    (cur, prev_list)
                },
            ),
        }
    }

    fn get_statement_type(
        &mut self,
        stmt: &Statement,
    ) -> (Option<IntermediateType>, Vec<IntermediateType>) {
        match stmt {
            Statement::Declaration(decl) => {
                self.visit_declaration(&mut decl.clone());
                (None, Vec::new())
            }
            Statement::BlockExpression(block) => self.get_block_expr_type(&**block),
            Statement::Expression(expr) => {
                self.visit_expression(&mut expr.clone());
                (None, self.get_type(expr).1)
            }
            Statement::Return(expr) => expr
                .as_ref()
                .map(|e| {
                    let (ty, mut ret_tys) = self.get_type(e);
                    ret_tys.push(ty);
                    (None, ret_tys)
                })
                .unwrap_or((None, Vec::new())),
        }
    }

    fn call_inferred(
        &mut self,
        inferred: &IntermediateType,
        args: &[Expression],
    ) -> IntermediateType {
        match inferred {
            IntermediateType::Exact(left) => self.call_exact(left, args),
            IntermediateType::Any(id) => {
                if let Some(ty) = self.infer_map.get(id).cloned() {
                    self.call_inferred(&ty, args)
                } else {
                    let any = self.get_any_ty();
                    let ty = TypeSignature::Function(
                        args.iter()
                            .map(|arg| Param::TypeOnly(Box::new(From::from(self.get_type(arg).0))))
                            .collect(),
                        Box::new(From::from(any.clone())),
                    );
                    self.make_equal(inferred, &From::from(ty));
                    any
                }
            }

            IntermediateType::Number(..) => panic!("Cannot call numbers"),
        }
    }

    fn call_exact(&mut self, ty: &TypeSignature, args: &[Expression]) -> IntermediateType {
        match ty {
            TypeSignature::Plain(name) => panic!("Cannot call type {}", name),
            TypeSignature::Pointer { ty, .. } => self.call_exact(ty, args),
            TypeSignature::Struct(_name, _fields) => unimplemented!(),
            TypeSignature::Enum(_name, _fields) => unimplemented!(),
            TypeSignature::Tuple(_tys) => unimplemented!(),
            TypeSignature::Array(_ty, _size) => unimplemented!(),
            TypeSignature::DynamicArray(_ty, _expr) => unimplemented!(),
            TypeSignature::Function(params, return_type) => {
                assert_eq!(args.len(), params.len());
                args.iter().zip(params.iter()).for_each(|(arg, param)| {
                    let arg_ty = self.get_type(arg).0;
                    let param_ty: TypeSignature = From::from(param.clone());
                    self.make_equal(&arg_ty, &From::from(param_ty));
                });
                From::from(*return_type.clone())
            }
            TypeSignature::Infer(_infer) => unimplemented!(),
        }
    }

    fn make_equal(&mut self, lvalue: &IntermediateType, rvalue: &IntermediateType) {
        if lvalue == rvalue {
            return;
        }
        match (lvalue, rvalue) {
            (IntermediateType::Exact(left), IntermediateType::Exact(right)) => {
                self.unify_types(left, right);
            }
            (lvalue, IntermediateType::Any(id)) => {
                if let Some(ty) = self.infer_map.get(id).cloned() {
                    self.make_equal(lvalue, &ty);
                } else {
                    self.infer_map.insert(*id, lvalue.clone());
                }
            }
            (IntermediateType::Any(id), rvalue) => {
                if let Some(ty) = self.infer_map.get(id).cloned() {
                    self.make_equal(&ty, rvalue);
                } else {
                    self.infer_map.insert(*id, rvalue.clone());
                }
            }

            (IntermediateType::Number(id_1, num_1), IntermediateType::Number(id_2, num_2)) =>
                self.make_equal_num(*id_1, num_1, *id_2, num_2),

            (IntermediateType::Number(id, num), IntermediateType::Exact(ty))
            | (IntermediateType::Exact(ty), IntermediateType::Number(id, num))
                => self.make_num_exact(*id, num, ty),
        }
    }

    fn make_num_exact(&mut self, id: i128, num: &IntermediateNumber, ty: &TypeSignature) {
        if let Some(num_ty) = self.infer_map.get(&id).cloned() {
            return self.make_equal(&num_ty, &IntermediateType::Exact(Box::new(ty.clone())));
        }
        let res_ty = match (num, ty) {
            (_, TypeSignature::Infer(i)) => return self.make_equal(&IntermediateType::Number(id, num.clone()), i),
            (IntermediateNumber::Indeterminate, TypeSignature::Plain(t)) => {
                match t.as_ref() {
                    "int" | "float" | "double" => TypeSignature::Plain(t.clone()),
                    otherwise => panic!("Not implemented: {:?}", otherwise),
                }
            }
            (IntermediateNumber::Float, TypeSignature::Plain(t)) => {
                match t.as_ref() {
                    "float" | "double" => TypeSignature::Plain(t.clone()),
                    otherwise => panic!("Not implemented: {:?}", otherwise),
                }
            }
            otherwise => panic!("Not implemented: {:?}", otherwise),
        };

        self.infer_map.insert(id, IntermediateType::Exact(Box::new(res_ty)));
    }

    fn make_equal_num(&mut self,
                      id_1: i128, num_1: &IntermediateNumber,
                      id_2: i128, num_2: &IntermediateNumber) {
        if let Some(ty) = self.infer_map.get(&id_1).cloned() {
            return self.make_equal(&ty, &IntermediateType::Number(id_2, num_2.clone()));
        } else if let Some(ty) = self.infer_map.get(&id_2).cloned() {
            return self.make_equal(&IntermediateType::Number(id_1, num_1.clone()), &ty);
        }
        match (num_1, num_2) {
            (IntermediateNumber::Indeterminate, t) => { self.infer_map.insert(id_1, IntermediateType::Number(id_2, t.clone())); }
            (t, IntermediateNumber::Indeterminate) => { self.infer_map.insert(id_2, IntermediateType::Number(id_1, t.clone())); }
            (IntermediateNumber::Float, IntermediateNumber::Float) => {}
            (IntermediateNumber::Double, IntermediateNumber::Double) => {}
            (IntermediateNumber::Float, IntermediateNumber::Double) => {
                let id = self.get_ty_index();
                self.infer_map.insert(id_1, IntermediateType::Number(id, IntermediateNumber::Double));
            }
            (IntermediateNumber::Double, IntermediateNumber::Float) => {
                let id = self.get_ty_index();
                self.infer_map.insert(id_2, IntermediateType::Number(id, IntermediateNumber::Double));
            }
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }

    fn make_equal_to_num(&mut self, ty: &IntermediateType) -> IntermediateType {
        let num = self.get_number_ty();
        self.make_equal(&ty, &num);
        num
    }

    fn unify_types(&mut self, lvalue: &TypeSignature, rvalue: &TypeSignature) {
        match lvalue {
            TypeSignature::Plain(_name) => self.fail_if(lvalue != rvalue, lvalue, rvalue),
            TypeSignature::Pointer { .. } => unimplemented!(),
            TypeSignature::Struct(_name, _fields) => unimplemented!(),
            TypeSignature::Enum(_name, _fields) => unimplemented!(),
            TypeSignature::Tuple(_tys) => unimplemented!(),
            TypeSignature::Array(_ty, _size) => unimplemented!(),
            TypeSignature::DynamicArray(_ty, _expr) => unimplemented!(),
            TypeSignature::Function(_params, _return_value) => unimplemented!(),
            TypeSignature::Infer(_infer) => unimplemented!(),
        }
    }

    fn fail_if(&self, cond: bool, left: &TypeSignature, right: &TypeSignature) {
        if cond {
            panic!("Cannot assign {:?} to {:?}", right, left)
        }
    }
}

struct TypeInserter<'a> {
    infer_map: &'a mut HashMap<i128, IntermediateType>,
}

impl ASTVisitor for TypeInserter<'_> {
    fn visit_ty(&mut self, s: &mut TypeSignature) {
        match s.clone() {
            TypeSignature::Infer(IntermediateType::Any(id)) => {
                if let Some(ty) = self.infer_map.get(&id).cloned() {
                    *s = From::from(ty);
                    self.visit_ty(s);
                } else {
                    panic!("Did not find type for id {}", id);
                }
            }
            TypeSignature::Infer(IntermediateType::Number(id, more)) => {
                if let Some(ty) = self.infer_map.get(&id).cloned() {
                    *s = From::from(ty);
                    self.visit_ty(s);
                } else {
                    match more {
                        _ => *s = TypeSignature::Plain(From::from("int"))
                    }
                }
            }
            _ => {}
        }
        walk_ty(self, s);
    }
}
