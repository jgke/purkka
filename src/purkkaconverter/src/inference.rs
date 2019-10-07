/// Infer types
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::ops::Deref;
use std::rc::Rc;

use crate::traits::TreeTransformer;
use crate::PurkkaToC;
use purkkasyntax::visitor::*;
use purkkasyntax::*;

#[derive(Debug)]
pub struct TypeInferrer<'a> {
    scope: Vec<HashMap<Rc<str>, IntermediateType>>,
    infer_map: HashMap<i128, IntermediateType>,
    context: &'a PurkkaToC,
    current_statement: String,
    current_expression: String,
    current_function: String,
}

impl<'a> TreeTransformer<'a> for TypeInferrer<'a> {
    fn new(context: &'a mut PurkkaToC) -> TypeInferrer {
        TypeInferrer {
            scope: vec![HashMap::new()],
            infer_map: HashMap::new(),
            context,
            current_statement: "".to_string(),
            current_expression: "".to_string(),
            current_function: "".to_string(),
        }
    }
    fn transform(&mut self, s: &mut S) {
        if let Err(res) = self.visit_s(s) {
            println!("{}", res);
            println!("Current statement: {}", self.current_statement);
            println!("Current expression: {}", self.current_expression);
            println!("Current declaration: {}", self.current_function);
            panic!();
        }

        let mut ti = TypeInserter {
            infer_map: &mut self.infer_map,
            current_statement: "".to_string(),
        };

        if let Err(res) = ti.visit_s(s) {
            println!("{}", res);
            println!("Current statement: {}", self.current_statement);
            panic!();
        }
    }
}

macro_rules! not_impl {
    ($arg:ident) => {
        Err(format!(
            "[{}:{}] Not yet implemented: {:?}",
            file!(),
            line!(),
            $arg
        ));
    };
    ($arg1:ident, $arg2:ident) => {
        Err(format!(
            "[{}:{}] Not yet implemented:\n  {:?}\n  {:?}",
            file!(),
            line!(),
            $arg1,
            $arg2
        ));
    };
}

impl ASTVisitor for TypeInferrer<'_> {
    unit_result!();
    type Err = String;

    fn visit_declaration(&mut self, tree: &mut Declaration) -> Result<(), String> {
        let Declaration::Declaration(_flags, exact_ty, decls) = tree;
        for (name, expr) in decls { 
            match expr {
                Some(e) => {
                    self.current_function = name.to_string();
                    self.push_symbol(name.clone(), From::from(*exact_ty.clone()));
                    let ty = self.get_type(e)?;
                    let intermediate: IntermediateType = From::from(*exact_ty.clone());
                    if !ty.1.is_empty() {
                        match &**exact_ty {
                            TypeSignature::Function(_, ret) => {
                                for ret_ty in ty.1 {
                                    self.make_equal(&ret_ty, &From::from(*ret.clone()))?;
                                }
                            }
                            otherwise => return not_impl!(otherwise),
                        }
                    }
                    self.make_equal(&ty.0, &intermediate)?;
                    self.push_symbol(name.clone(), intermediate);
                }
                None => {
                    self.push_symbol(name.clone(), From::from(*exact_ty.clone()));
                }
            }
        }
        walk_declaration(self, tree)
    }

    fn visit_operator_overload(&mut self, s: &mut OperatorOverload) -> Result<(), String> {
        match s {
            OperatorOverload::OperatorOverload(_ident, ty, expr) => {
                let expr_ty = self.get_type(expr)?;
                self.make_equal(&From::from(*ty.clone()), &expr_ty.0)?;
            }
        }
        walk_operator_overload(self, s)
    }

    fn visit_lambda(&mut self, tree: &mut Lambda) -> Result<(), String> {
        self.push_block();
        let Lambda::Lambda(params, _, e) = tree;
        for param in params {
            match param {
                LambdaParam::LambdaParam(name, ty) => {
                    self.push_symbol(name.clone(), From::from(*ty.clone()))
                }
                LambdaParam::Variadic => {}
            }
        }
        let res = walk_block(self, e);
        self.pop_block();
        res
    }

    fn visit_block(&mut self, e: &mut Block) -> Result<(), String> {
        self.push_block();
        let res = walk_block(self, e);
        self.pop_block();
        res
    }

    fn visit_primary_expression(&mut self, e: &mut PrimaryExpression) -> Result<(), String> {
        self.push_block();
        if let PrimaryExpression::Lambda(Lambda::Lambda(params, _, _)) = e {
            for param in params {
                match param {
                    LambdaParam::LambdaParam(name, ty) => {
                        self.push_symbol(name.clone(), From::from(*ty.clone()))
                    }
                    LambdaParam::Variadic => {}
                }
            }
        }
        let res = walk_primary_expression(self, e);
        self.pop_block();
        res
    }
}

impl TypeInferrer<'_> {
    fn push_block(&mut self) {
        self.scope.push(HashMap::new());
    }

    fn push_symbol(&mut self, name: Rc<str>, ty: IntermediateType) {
        self.scope.last_mut().unwrap().insert(name, ty);
    }

    fn add_type_alias(&mut self, id: i128, ty: IntermediateType) -> Result<(), String> {
        if self.infer_map.contains_key(&id) {
            unimplemented!();
        }
        self.infer_map.insert(id, ty);
        Ok(())
    }

    fn pop_block(&mut self) {
        self.scope.pop();
    }

    fn get_symbol_ty(&self, t: &str) -> Option<IntermediateType> {
        for tys in self.scope.iter().rev() {
            if let Some(ty) = tys.get(t) {
                return Some(ty.clone());
            }
        }
        if let Some(ty) = self.context.symbols.imported_declarations.0.get(t) {
            Some(From::from(ty.clone()))
        } else {
            None
        }
    }

    fn get_ident_ty(&self, name: &str) -> Option<TypeSignature> {
        self.context
            .symbols
            .types.0
            .get(name)
            .map(|t| &t.0)
            .or_else(|| self.context.symbols.imported_types.0.get(name))
            .cloned()
    }

    fn get_any_ty(&mut self) -> IntermediateType {
        IntermediateType::new_any()
    }

    fn get_number_ty(&mut self) -> IntermediateType {
        IntermediateType::new_number(IntermediateNumber::Indeterminate)
    }

    fn get_operator_instance(&self, op: &Rc<str>) -> (Vec<IntermediateType>, IntermediateType) {
        let op = self.context.operators.infix.get(op).unwrap();

        match &op.ty {
            TypeSignature::Function(args, ret) => {
                let mut any_ty = None;
                let mut num_ty = None;

                let args_tys = args
                    .iter()
                    .map(|arg| match arg.ty_field() {
                        TypeSignature::Infer(IntermediateType::Any(0)) => {
                            any_ty.get_or_insert_with(IntermediateType::new_any).clone()
                        }
                        TypeSignature::Infer(IntermediateType::Number(0, num)) => num_ty
                            .get_or_insert_with(|| IntermediateType::new_number(*num))
                            .clone(),
                        ty => From::from(ty.clone()),
                    })
                    .collect();

                let ret = match &**ret {
                    TypeSignature::Infer(IntermediateType::Any(0)) => {
                        any_ty.unwrap_or_else(IntermediateType::new_any).clone()
                    }
                    TypeSignature::Infer(IntermediateType::Number(0, num)) => {
                        num_ty.unwrap_or_else(|| IntermediateType::new_number(*num))
                    }
                    ty => From::from(ty.clone()),
                };
                (args_tys, ret)
            }
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }

    fn get_unary_operator_instance(
        &self,
        op: &Rc<str>,
    ) -> (Vec<IntermediateType>, IntermediateType) {
        let op = self.context.operators.unary.get(op).unwrap();

        match &op.ty {
            TypeSignature::Function(args, ret) => {
                let mut any_ty = None;
                let mut num_ty = None;

                let args_tys = args
                    .iter()
                    .map(|arg| match arg.ty_field() {
                        TypeSignature::Infer(IntermediateType::Any(0)) => {
                            any_ty.get_or_insert_with(IntermediateType::new_any).clone()
                        }
                        TypeSignature::Infer(IntermediateType::Number(0, num)) => num_ty
                            .get_or_insert_with(|| IntermediateType::new_number(*num))
                            .clone(),
                        ty => From::from(ty.clone()),
                    })
                    .collect();

                let ret = match &**ret {
                    TypeSignature::Infer(IntermediateType::Any(0)) => {
                        any_ty.unwrap_or_else(IntermediateType::new_any).clone()
                    }
                    TypeSignature::Infer(IntermediateType::Number(0, num)) => {
                        num_ty.unwrap_or_else(|| IntermediateType::new_number(*num))
                    }
                    ty => From::from(ty.clone()),
                };
                (args_tys, ret)
            }
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }

    fn get_postfix_operator_instance(&self, op: &Rc<str>) -> (IntermediateType, IntermediateType) {
        let op = self.context.operators.postfix.get(op).unwrap();

        match &op.ty {
            TypeSignature::Function(args, ret) => {
                assert_eq!(args.len(), 1);

                let mut any_ty = None;
                let mut num_ty = None;

                let arg = &args[0];
                let arg_ty = match arg.ty_field() {
                    TypeSignature::Infer(IntermediateType::Any(0)) => {
                        any_ty.get_or_insert_with(IntermediateType::new_any).clone()
                    }
                    TypeSignature::Infer(IntermediateType::Number(0, num)) => num_ty
                        .get_or_insert_with(|| IntermediateType::new_number(*num))
                        .clone(),
                    ty => From::from(ty.clone()),
                };

                let ret = match &**ret {
                    TypeSignature::Infer(IntermediateType::Any(0)) => {
                        any_ty.unwrap_or_else(IntermediateType::new_any).clone()
                    }
                    TypeSignature::Infer(IntermediateType::Number(0, num)) => {
                        num_ty.unwrap_or_else(|| IntermediateType::new_number(*num))
                    }
                    ty => From::from(ty.clone()),
                };
                (arg_ty, ret)
            }
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }

    fn get_type(
        &mut self,
        expression: &mut Expression,
    ) -> Result<(IntermediateType, Vec<IntermediateType>), String> {
        self.current_expression = format!("{:?}", expression);
        //println!("{:?}\n", expression);
        let mut ty = match expression {
            Expression::PrimaryExpression(expr) => self.get_primary_expr_type(expr)?,
            Expression::Op(op, ExprList::List(list)) => {
                /*
                 * We special case handling for '+', '-' and '?' here, because they're, well,
                 * special. '+' and '-' are especially special, as they are the only overloaded
                 * operators in C - they have interesting behaviour with pointers.
                 *
                 * num + ptr -> ptr
                 * ptr + num -> ptr
                 * num + num -> num
                 *
                 * ptr - num -> ptr
                 * ptr - ptr -> num
                 * num - num -> num
                 */
                if op.as_ref() == "+" {
                    let left = &mut list[0];
                    let left_ty = self.get_type(left)?;

                    let right = &mut list[1];
                    let right_ty = self.get_type(right)?;

                    let left_ptr = left_ty.0.is_ptr(&self.infer_map);
                    let right_ptr = right_ty.0.is_ptr(&self.infer_map);

                    let ret_tys = left_ty
                        .1
                        .into_iter()
                        .chain(right_ty.1.into_iter())
                        .collect();

                    if left_ptr && !right_ptr {
                        self.make_equal_to_num(&right_ty.0)?;
                        (left_ty.0, ret_tys)
                    } else if !left_ptr && right_ptr {
                        self.make_equal_to_num(&left_ty.0)?;
                        (right_ty.0, ret_tys)
                    } else if left_ptr && right_ptr {
                        return Err("Cannot add two pointers together".to_string());
                    } else {
                        // assume num + num here
                        self.make_equal_to_num(&left_ty.0)?;
                        self.make_equal(&left_ty.0, &right_ty.0)?;
                        (left_ty.0, ret_tys)
                    }
                } else if op.as_ref() == "-" {
                    let left = &mut list[0];
                    let left_ty = self.get_type(left)?;
                    let left_ptr = left_ty.0.is_ptr(&self.infer_map);

                    let right = &mut list[1];
                    let right_ty = self.get_type(right)?;
                    let right_ptr = right_ty.0.is_ptr(&self.infer_map);

                    let ret_tys = left_ty
                        .1
                        .into_iter()
                        .chain(right_ty.1.into_iter())
                        .collect();

                    if left_ptr && right_ptr {
                        self.make_equal(&left_ty.0, &right_ty.0)?;
                        (From::from(TypeSignature::long()), ret_tys)
                    } else if left_ptr {
                        self.make_equal_to_num(&right_ty.0)?;
                        (left_ty.0, ret_tys)
                    } else {
                        // assume num - num here
                        self.make_equal_to_num(&left_ty.0)?;
                        self.make_equal(&left_ty.0, &right_ty.0)?;
                        (left_ty.0, ret_tys)
                    }
                } else if op.as_ref() == "?" {
                    let cond = &mut list[0];
                    let cond_ty = self.get_type(cond)?;

                    let if_t = &mut list[1];
                    let if_t_ty = self.get_type(if_t)?;

                    let if_f = &mut list[2];
                    let if_f_ty = self.get_type(if_f)?;

                    self.make_equal_to_num(&cond_ty.0)?;

                    self.make_equal(&if_t_ty.0, &if_f_ty.0)?;

                    let ret_tys = cond_ty
                        .1
                        .into_iter()
                        .chain(if_t_ty.1.into_iter())
                        .chain(if_f_ty.1.into_iter())
                        .collect();

                    (if_t_ty.0, ret_tys)
                } else {
                    let (mut params, ret) = self.get_operator_instance(op);

                    assert_eq!(params.len(), list.len());
                    let ret_tys = params
                        .iter_mut()
                        .zip(list.iter_mut())
                        .map(|(param, arg)| {
                            let arg_ty = self.get_type(arg)?;
                            self.make_equal(&arg_ty.0, param)?;
                            Ok(arg_ty.1)
                        })
                        .collect::<Result<Vec<Vec<IntermediateType>>, String>>()?
                        .into_iter()
                        .flatten()
                        .collect();

                    (ret, ret_tys)
                }
            }
            Expression::Unary(op, ExprList::List(list)) => {
                if op.as_ref() == "*" {
                    assert_eq!(list.len(), 1);
                    let arg_ty = self.get_type(&mut list[0])?;
                    if arg_ty.0.is_ptr(&self.infer_map) {
                        (
                            IntermediateType::Exact(Box::new(
                                arg_ty.0.dereference(&self.infer_map).unwrap(),
                            )),
                            arg_ty.1,
                        )
                    } else {
                        return Err("Cannot dereference non-pointer types".to_string());
                    }
                } else if op.as_ref() == "&" {
                    assert_eq!(list.len(), 1);
                    let arg_ty = self.get_type(&mut list[0])?;
                    (arg_ty.0.address_of(), arg_ty.1)
                } else {
                    let (mut params, ret) = self.get_unary_operator_instance(op);

                    assert_eq!(params.len(), list.len());
                    let ret_tys = params
                        .iter_mut()
                        .zip(list.iter_mut())
                        .map(|(param, arg)| {
                            let arg_ty = self.get_type(arg)?;
                            self.make_equal(&arg_ty.0, param)?;
                            Ok(arg_ty.1)
                        })
                        .collect::<Result<Vec<Vec<IntermediateType>>, String>>()?
                        .into_iter()
                        .flatten()
                        .collect();

                    (ret, ret_tys)
                }
            }
            Expression::PostFix(expr, op) => {
                let (param, ret) = self.get_postfix_operator_instance(op);

                let arg_ty = self.get_type(expr)?;
                self.make_equal(&arg_ty.0, &param)?;

                (ret, arg_ty.1)
            }
            Expression::ArrayAccess(expr, index) => self.get_array_access_type(expr, index)?,
            Expression::Call(expr, args) => {
                let (ty, mut ret_tys) = self.get_type(&mut *expr)?;
                for arg in args.iter_mut() {
                    let (_, mut more_ret_tys) = self.get_type(arg)?;
                    ret_tys.append(&mut more_ret_tys);
                }

                match From::from(ty) {
                    t @ TypeSignature::Function(..) => (self.call_exact(&t, args)?, ret_tys),
                    TypeSignature::Infer(inferred) => {
                        (self.call_inferred(&inferred, args)?, ret_tys)
                    }
                    otherwise => panic!("Not implemented: {:?}", otherwise),
                }
            }
            Expression::Cast(expr, ty) => (From::from(ty.clone()), self.get_type(expr)?.1),
            Expression::StructAccess(ref mut expr, ident) => self.struct_access(expr, ident)?,
            Expression::Sizeof(size) => {
                match size {
                    Sizeof::Expression(expr) => {
                        self.get_type(expr)?;
                    }
                    Sizeof::Type(_) => {}
                }

                (From::from(TypeSignature::size_t()), Vec::new())
            }
        };

        loop {
            match ty.0 {
                IntermediateType::Any(id) if self.infer_map.contains_key(&id) => {
                    ty.0 = self.infer_map[&id].clone()
                }
                _ => break,
            }
        }

        Ok(ty)
    }

    fn struct_access(
        &mut self,
        expr: &mut Expression,
        ident: &Rc<str>,
    ) -> Result<(IntermediateType, Vec<IntermediateType>), String> {
        let (struct_ty, ret_ty) = self.get_type(expr)?;
        Ok((self.struct_ty_access(expr, struct_ty, ident)?, ret_ty))
    }

    fn struct_ty_access(
        &mut self,
        e: &mut Expression,
        mut struct_ty: IntermediateType,
        ident: &Rc<str>,
    ) -> Result<IntermediateType, String> {
        while let IntermediateType::Exact(box TypeSignature::Plain(name)) = &struct_ty {
            if let Some(ty) = self.get_ident_ty(name) {
                struct_ty = From::from(ty);
            } else {
                panic!("Cannot access field {} from type {}", ident, name)
            }
        }
        match struct_ty {
            IntermediateType::Exact(box TypeSignature::Struct(s, fields)) => {
                for StructField { name, ty, .. } in &fields {
                    if name.as_ref() == ident.as_ref() {
                        return Ok(From::from(*ty.clone()));
                    }
                }
                if let Some(s) = s {
                    panic!("Field {} not found in struct {}", ident, s);
                } else {
                    panic!(
                        "Field {} not found in anonymous struct {:?}",
                        ident,
                        TypeSignature::Struct(s, fields)
                    );
                }
            }
            IntermediateType::Exact(box TypeSignature::Pointer { ty, .. }) => {
                let ret_val = self.struct_ty_access(e, From::from(*ty.clone()), ident)?;
                let mut new_e = Expression::PrimaryExpression(PrimaryExpression::Literal(
                    Literal::Integer(0),
                ));
                std::mem::swap(&mut new_e, e);
                *e = Expression::Unary(From::from("*"), ExprList::List(vec![new_e]));
                Ok(ret_val)
            }
            IntermediateType::Any(id) => {
                if let Some(ty) = self.infer_map.get(&id).cloned() {
                    self.struct_ty_access(e, ty, ident)
                } else {
                    panic!("Cannot access field {} in {:?}", ident, struct_ty);
                }
            }
            _ => panic!("Cannot access field {} in {:?}", ident, struct_ty),
        }
    }

    fn get_primary_expr_type(
        &mut self,
        expr: &mut PrimaryExpression,
    ) -> Result<(IntermediateType, Vec<IntermediateType>), String> {
        match expr {
            PrimaryExpression::Identifier(t) => {
                let sym_ty = self
                    .get_symbol_ty(t.as_ref())
                    .unwrap_or_else(|| panic!("Unknown identifier: {}", t));
                Ok((sym_ty, Vec::new()))
            }
            PrimaryExpression::Literal(lit) => match lit {
                Literal::Integer(..) => Ok((self.get_number_ty(), Vec::new())),
                Literal::Float(..) => Ok((
                    IntermediateType::new_number(IntermediateNumber::Float),
                    Vec::new(),
                )),
                Literal::StringLiteral(..) => Ok((
                    IntermediateType::Exact(Box::new(TypeSignature::Pointer {
                        nullable: false,
                        ty: Box::new(TypeSignature::Primitive(Primitive::Char)),
                    })),
                    Vec::new(),
                )),
                Literal::Char(..) => Ok((
                    IntermediateType::Exact(Box::new(TypeSignature::Primitive(Primitive::Char))),
                    Vec::new(),
                )),
            },
            PrimaryExpression::Lambda(Lambda::Lambda(params, return_type, block)) => {
                self.push_block();
                for param in params.iter() {
                    match param {
                        LambdaParam::LambdaParam(name, ty) => {
                            self.push_symbol(name.clone(), From::from(*ty.clone()))
                        }
                        LambdaParam::Variadic => {}
                    }
                }
                let block_ty = self.get_block_type(&mut *block)?;
                let ret_tys = block_ty.1;
                if let Some((last, rest)) = ret_tys.split_last() {
                    for ty in rest {
                        self.make_equal(&ty, &last)?;
                    }
                }
                let ret_ty = if let Some(ty) = block_ty.0 {
                    if let Some(ret_ty) = ret_tys.get(0) {
                        self.make_equal(&ty, &ret_ty)?;
                    }
                    ty
                } else if let Some(ty) = ret_tys.get(0) {
                    ty.clone()
                } else {
                    From::from(TypeSignature::Primitive(Primitive::Void))
                };
                self.make_equal(&From::from(return_type.clone()), &ret_ty)?;
                let ty = From::from(TypeSignature::Function(
                    params.iter().cloned().map(From::from).collect(),
                    Box::new(From::from(ret_ty)),
                ));
                self.pop_block();
                Ok((ty, Vec::new()))
            }
            PrimaryExpression::Expression(expr) => self.get_type(expr),
            PrimaryExpression::StructInitialization(ident, fields) => {
                let struct_ty = self
                    .context
                    .symbols
                    .types.0
                    .get(ident)
                    .map(|t| &t.0)
                    .unwrap_or_else(|| &self.context.symbols.imported_types.0[ident]);
                if let TypeSignature::Struct(struct_name, struct_fields) = struct_ty {
                    let mut used_names: HashSet<Rc<str>> = HashSet::new();
                    let mut remaining_names: Vec<Rc<str>> = struct_fields
                        .iter()
                        .map(|StructField { name, .. }| name.clone())
                        .rev()
                        .collect::<Vec<_>>();
                    let mut struct_field_tys = struct_fields
                        .iter()
                        .map(|StructField { ty, name, .. }| (name, ty))
                        .collect::<Vec<_>>();
                    struct_field_tys.sort_by(|(l_name, _), (r_name, _)| l_name.cmp(&r_name));

                    let mut arg_field_tys = fields
                        .iter_mut()
                        .map(
                            |StructInitializationField::StructInitializationField(
                                ref mut name,
                                e,
                            )| {
                                if name.is_none() {
                                    let res = remaining_names.pop().unwrap_or_else(|| {
                                        panic!(format!(
                                            "Struct {:?} has only {} fields",
                                            struct_name,
                                            used_names.len()
                                        ))
                                    });
                                    used_names.insert(res.clone());
                                    *name = Some(res);
                                } else {
                                    let ident = name.as_ref().unwrap();
                                    if !remaining_names.contains(&ident) {
                                        if used_names.contains(ident) {
                                            panic!(
                                                "Cannot instantiate field {} more than once",
                                                ident
                                            );
                                        } else {
                                            panic!(
                                                "Field {} does not exist in struct {:?}",
                                                ident, struct_name
                                            );
                                        }
                                    }
                                    remaining_names
                                        .iter()
                                        .position(|name| name == ident)
                                        .map(|i| remaining_names.remove(i));

                                    used_names.insert(ident.clone());
                                }
                                Ok((name.as_mut().unwrap(), self.get_type(&mut **e)?))
                            },
                        )
                        .collect::<Result<
                            Vec<(&mut Rc<str>, (IntermediateType, Vec<IntermediateType>))>,
                            String,
                        >>()?;
                    arg_field_tys.sort_by(|(l_name, _), (r_name, _)| l_name.cmp(&r_name));

                    let mut ret_tys = Vec::new();

                    // XXX: another C hack
                    // if struct_field_tys.len() != arg_field_tys.len() {
                    if struct_field_tys.len() < arg_field_tys.len() {
                        return Err(format!(
                            "Expected {} struct initialization fields, but got {}",
                            struct_field_tys.len(),
                            arg_field_tys.len()
                        ));
                    }

                    for (i, (name, (e_ty, mut ret_ty))) in arg_field_tys.into_iter().enumerate() {
                        let (struct_field_name, struct_field_ty) = &struct_field_tys[i];
                        assert_eq!(&name, struct_field_name);
                        self.make_equal(&From::from(*struct_field_ty.deref().clone()), &e_ty)?;
                        ret_tys.append(&mut ret_ty);
                    }
                    Ok((From::from(TypeSignature::Plain(ident.clone())), ret_tys))
                } else {
                    panic!(
                        "Cannot instantiate non-struct type {:?} as a struct",
                        struct_ty
                    );
                }
            }
            PrimaryExpression::VectorInitialization(ident, fields) => {
                let vec_ty = self
                    .context
                    .symbols
                    .types.0
                    .get(ident)
                    .map(|t| &t.0)
                    .unwrap_or_else(|| &self.context.symbols.imported_types.0[ident]);
                match vec_ty {
                    TypeSignature::Vector(plain)
                        | TypeSignature::Attribute(box TypeSignature::Vector(plain), _) => {
                            let prim = From::from(TypeSignature::Primitive(*plain));
                            let mut ret_tys = Vec::new();
                            for expr in fields {
                                let (e_ty, mut ret) = self.get_type(expr)?;
                                ret_tys.append(&mut ret);
                                self.make_equal(&prim, &e_ty)?;
                            }
                            Ok((From::from(TypeSignature::Plain(ident.clone())), ret_tys))
                        }
                    _ => {
                        panic!(
                            "Cannot instantiate non-vector type {:?} as a vector",
                            vec_ty
                            );
                    }
                }
            }
            PrimaryExpression::BlockExpression(block) => {
                let (l, r) = self.get_block_expr_type(block)?;
                Ok((l.unwrap_or_else(|| From::from(TypeSignature::void())), r))
            }
            PrimaryExpression::ArrayLiteral(exprs) => {
                let (l, r) = if let Some((l, r)) = self.fold_expr_list(exprs)? {
                    (l, r)
                } else {
                    (IntermediateType::new_any(), Vec::new())
                };
                Ok((
                    From::from(TypeSignature::Array(
                        Box::new(From::from(l)),
                        Some(exprs.len()),
                    )),
                    r,
                ))
            }
        }
    }

    fn fold_expr_list(
        &mut self,
        exprs: &mut [Expression],
    ) -> Result<Option<(IntermediateType, Vec<IntermediateType>)>, String> {
        if let Some((first, rest)) = exprs.split_first_mut() {
            let (ty, mut ret_tys) = self.get_type(first)?;
            for e in rest {
                let (next_ty, mut more_ret_tys) = self.get_type(e)?;
                self.make_equal(&ty, &next_ty)?;
                ret_tys.append(&mut more_ret_tys);
            }
            Ok(Some((ty, ret_tys)))
        } else {
            Ok(None)
        }
    }

    fn get_array_access_type(
        &mut self,
        array_expr: &mut Expression,
        index_expr: &mut Expression,
    ) -> Result<(IntermediateType, Vec<IntermediateType>), String> {
        let (ty, mut ret_tys) = self.get_type(array_expr)?;
        let (index_ty, index_ret_tys) = self.get_type(index_expr)?;
        ret_tys.extend(index_ret_tys);
        self.make_equal_to_num(&index_ty)?;
        Ok((self.array_access_inferred(&ty, index_expr)?, ret_tys))
    }

    fn array_access_exact(
        &mut self,
        array_ty: &TypeSignature,
        index_expr: &Expression,
    ) -> Result<IntermediateType, String> {
        match array_ty {
            TypeSignature::Plain(name) => {
                if let Some(ty) = self.get_ident_ty(name) {
                    self.array_access_exact(&ty, index_expr)
                } else {
                    panic!("Cannot index into type {}", name)
                }
            }
            TypeSignature::Primitive(prim) => panic!("Cannot index into type {}", prim),
            TypeSignature::Vector(plain)
                | TypeSignature::Attribute(box TypeSignature::Vector(plain), _) =>
                Ok(From::from(TypeSignature::Primitive(*plain))),
            TypeSignature::Tuple(list) => {
                let lit = index_expr
                    .eval(&HashMap::new())
                    .expect("Tuple index must be a constant expression");
                let index = match lit {
                    Literal::Integer(i) => i,
                    otherwise => panic!(
                        "Tuple index must be a integer constant expression (got {:?})",
                        otherwise
                    ),
                };
                Ok(From::from(list[usize::try_from(index).unwrap()].clone()))
            }
            TypeSignature::Pointer { ty, .. }
            | TypeSignature::Array(ty, _)
            | TypeSignature::DynamicArray(ty, _) => Ok(From::from(*ty.clone())),
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }

    fn array_access_inferred(
        &mut self,
        inferred: &IntermediateType,
        index_expr: &Expression,
    ) -> Result<IntermediateType, String> {
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
                    self.add_type_alias(*id, arr_ty.clone())?;
                    Ok(arr_ty)
                }
            }
        }
    }

    fn get_block_expr_type(
        &mut self,
        expr: &mut BlockExpression,
    ) -> Result<(Option<IntermediateType>, Vec<IntermediateType>), String> {
        //println!("{:?}\n", expr);
        match expr {
            BlockExpression::Block(block) => self.get_block_type(&mut *block),
            BlockExpression::If(arms, otherwise) => {
                let mut res_vec = Vec::new();

                for (expr, b) in arms {
                    let expr_ty = self.get_type(expr)?;
                    self.make_equal_to_num(&expr_ty.0)?;
                    res_vec.push(self.get_block_type(b)?);
                }

                if let Some(otherwise) = otherwise {
                    let (otherwise_ty, mut ret_vals) = self.get_block_type(otherwise)?;

                    for (this, more_ret_vals) in res_vec {
                        if let Some(t) = &otherwise_ty {
                            self.make_equal(&this.as_ref().unwrap(), t)?;
                        }
                        ret_vals.append(&mut more_ret_vals.clone());
                    }

                    Ok((otherwise_ty, ret_vals))
                } else {
                    Ok((
                        None,
                        res_vec.into_iter().flat_map(|t| t.1.into_iter()).collect(),
                    ))
                }
            }
            BlockExpression::While(e, block, else_block, _is_do_while) => {
                let (ty, mut ret_tys) = self.get_type(&mut *e)?;
                let num = self.get_number_ty();
                self.make_equal(&ty, &num)?;

                let (_, more_ret_tys) = self.get_block_type(block)?;
                let even_more_ret_tys: Vec<IntermediateType> = else_block
                    .as_mut()
                    .map::<Result<Vec<IntermediateType>, String>, _>(|b| {
                        Ok(self.get_block_type(b)?.1)
                    })
                    .transpose()?
                    .unwrap_or_else(Vec::new);
                ret_tys.extend(more_ret_tys);
                ret_tys.extend(even_more_ret_tys);
                Ok((None, ret_tys))
            }
            BlockExpression::For(s1, s2, s3, block, else_block) => {
                self.push_block();
                let num = self.get_number_ty();

                let mut ret_tys = s1
                    .as_mut()
                    .map::<Result<Vec<IntermediateType>, String>, _>(|s| {
                        Ok(self.get_statement_type(&mut *s)?.1)
                    })
                    .transpose()?
                    .unwrap_or_else(Vec::new);
                let (ty, ret_tys_2) = s2
                    .as_mut()
                    .map::<Result<(IntermediateType, Vec<IntermediateType>), String>, _>(|s| {
                        Ok(self.get_type(&mut *s)?)
                    })
                    .transpose()?
                    .unwrap_or_else(|| (num.clone(), Vec::new()));
                self.make_equal(&ty, &num)?;
                let ret_tys_3 = s3
                    .as_mut()
                    .map::<Result<Vec<IntermediateType>, String>, _>(|s| {
                        Ok(self.get_type(&mut *s)?.1)
                    })
                    .transpose()?
                    .unwrap_or_else(Vec::new);

                let (_, ret_tys_4) = self.get_block_type(block)?;
                let ret_tys_5 = else_block
                    .as_mut()
                    .map::<Result<Vec<IntermediateType>, String>, _>(|b| {
                        Ok(self.get_block_type(b)?.1)
                    })
                    .transpose()?
                    .unwrap_or_else(Vec::new);

                ret_tys.extend(ret_tys_2);
                ret_tys.extend(ret_tys_3);
                ret_tys.extend(ret_tys_4);
                ret_tys.extend(ret_tys_5);

                self.pop_block();

                Ok((None, ret_tys))
            }
        }
    }

    fn get_block_type(
        &mut self,
        expr: &mut Block,
    ) -> Result<(Option<IntermediateType>, Vec<IntermediateType>), String> {
        match expr {
            Block::Statements(stmts) => {
                let mut last_ty = None;
                let mut ret_tys = Vec::new();
                for stmt in stmts {
                    let (ty, more_ret_tys) = self.get_statement_type(stmt)?;
                    last_ty = ty;
                    ret_tys.extend(more_ret_tys);
                }
                Ok((last_ty, ret_tys))
            }
        }
    }

    fn get_statement_type(
        &mut self,
        stmt: &mut Statement,
    ) -> Result<(Option<IntermediateType>, Vec<IntermediateType>), String> {
        self.current_statement = format!("{:?}", stmt);
        //println!("{:?}\n", &stmt);
        match stmt {
            Statement::Declaration(decl) => {
                self.visit_declaration(&mut decl.clone())?;
                Ok((None, Vec::new()))
            }
            Statement::BlockExpression(block) => self.get_block_expr_type(&mut **block),
            Statement::Expression(expr) => {
                self.visit_expression(&mut expr.clone())?;
                Ok((None, self.get_type(expr)?.1))
            }
            Statement::Return(expr) => expr
                .as_mut()
                .map(|e| {
                    let (ty, mut ret_tys) = self.get_type(e)?;
                    ret_tys.push(ty);
                    Ok((None, ret_tys))
                })
                .unwrap_or_else(|| Ok((None, Vec::new()))),
            Statement::Pragma(_pragma) => Ok((None, Vec::new())),
            Statement::Jump(_jump) => Ok((None, Vec::new())),
        }
    }

    fn call_inferred(
        &mut self,
        inferred: &IntermediateType,
        args: &mut [Expression],
    ) -> Result<IntermediateType, String> {
        match inferred {
            IntermediateType::Exact(left) => self.call_exact(left, args),
            IntermediateType::Any(id) => {
                if let Some(ty) = self.infer_map.get(id).cloned() {
                    self.call_inferred(&ty, args)
                } else {
                    let any = self.get_any_ty();
                    let mut fn_args = Vec::new();
                    for arg in args.iter_mut() {
                        fn_args.push(Param::TypeOnly(Box::new(From::from(self.get_type(arg)?.0))));
                    }
                    let ty = TypeSignature::Function(fn_args, Box::new(From::from(any.clone())));
                    self.make_equal(inferred, &From::from(ty))?;
                    Ok(any)
                }
            }

            IntermediateType::Number(..) => panic!("Cannot call numbers"),
        }
    }

    fn call_exact(
        &mut self,
        ty: &TypeSignature,
        args: &mut [Expression],
    ) -> Result<IntermediateType, String> {
        match ty {
            TypeSignature::Plain(name) => {
                if let Some(ty) = self.get_ident_ty(name) {
                    self.call_exact(&ty, args)
                } else {
                    panic!("Cannot call type {}", name)
                }
            }
            TypeSignature::Primitive(prim)
            | TypeSignature::Vector(prim)
            | TypeSignature::Complex(prim) => panic!("Cannot call type {}", prim),
            TypeSignature::Pointer { ty, .. } => self.call_exact(ty, args),
            TypeSignature::Struct(_name, _fields) => not_impl!(ty),
            TypeSignature::Enum(_name, _fields) => not_impl!(ty),
            TypeSignature::Union(_name, _fields) => not_impl!(ty),
            TypeSignature::Tuple(_tys) => not_impl!(ty),
            TypeSignature::Array(_ty, _size) => not_impl!(ty),
            TypeSignature::DynamicArray(_ty, _expr) => not_impl!(ty),
            TypeSignature::Function(params, return_type) => {
                if params.last() != Some(&Param::Variadic) {
                    if args.len() != params.len() {
                        return Err(format!(
                            "Expected {} arguments, but got {}",
                            params.len(),
                            args.len()
                        ));
                    }
                } else if args.len() < params.len() - 1 {
                    return Err(format!(
                        "Expected at least {} arguments, but got {}",
                        params.len() - 1,
                        args.len()
                    ));
                }
                for (arg, param) in args.iter_mut().zip(params.iter()) {
                    if param != &Param::Variadic {
                        let arg_ty = self.get_type(arg)?.0;
                        let param_ty: TypeSignature = From::from(param.clone());
                        self.make_equal(&From::from(param_ty), &arg_ty)?;
                    }
                }
                Ok(From::from(*return_type.clone()))
            }
            TypeSignature::Infer(_infer) => not_impl!(ty),
            TypeSignature::Attribute(ty, attrs) => not_impl!(ty, attrs),
        }
    }

    fn make_equal(
        &mut self,
        lvalue: &IntermediateType,
        rvalue: &IntermediateType,
    ) -> Result<(), String> {
        if lvalue == rvalue {
            return Ok(());
        }
        match (lvalue, rvalue) {
            (IntermediateType::Exact(left), IntermediateType::Exact(right)) => {
                self.unify_types(left, right)
            }
            (IntermediateType::Number(id, num), IntermediateType::Exact(ty))
            | (IntermediateType::Exact(ty), IntermediateType::Number(id, num)) => {
                self.make_num_exact(*id, num, ty)
            }
            (intermediate, IntermediateType::Exact(ty)) => self.unify_with_infer(intermediate, ty),
            (IntermediateType::Exact(ty), intermediate) => self.unify_with_infer(intermediate, ty),
            (lvalue, IntermediateType::Any(id)) => {
                if let Some(ty) = self.infer_map.get(id).cloned() {
                    self.make_equal(lvalue, &ty)
                } else {
                    self.add_type_alias(*id, lvalue.clone())
                }
            }
            (IntermediateType::Any(id), rvalue) => {
                if let Some(ty) = self.infer_map.get(id).cloned() {
                    self.make_equal(&ty, rvalue)
                } else {
                    self.add_type_alias(*id, rvalue.clone())
                }
            }

            (IntermediateType::Number(id_1, num_1), IntermediateType::Number(id_2, num_2)) => {
                self.make_equal_num(*id_1, num_1, *id_2, num_2)
            }
        }
    }

    fn unify_with_infer(
        &mut self,
        lvalue: &IntermediateType,
        rvalue: &TypeSignature,
    ) -> Result<(), String> {
        match (lvalue, rvalue) {
            (left, TypeSignature::Infer(right)) => self.make_equal(left, right),
            (IntermediateType::Exact(left), right) => self.unify_types(left, right),
            (IntermediateType::Any(id), rvalue) => {
                if let Some(ty) = self.infer_map.get(id).cloned() {
                    self.unify_with_infer(&ty, rvalue)
                } else {
                    self.add_type_alias(*id, From::from(rvalue.clone()))
                }
            }

            (IntermediateType::Number(id, num), ty) => self.make_num_exact(*id, num, ty),
        }
    }

    fn make_num_exact(
        &mut self,
        id: i128,
        num: &IntermediateNumber,
        ty: &TypeSignature,
    ) -> Result<(), String> {
        if let Some(num_ty) = self.infer_map.get(&id).cloned() {
            return self.unify_with_infer(&num_ty, &ty);
        }
        if let TypeSignature::Plain(name) = ty {
            if let Some(ty) = self.get_ident_ty(name) {
                return self.make_num_exact(id, num, &ty);
            }
        }

        let res_ty = match (num, ty) {
            (_, TypeSignature::Infer(i)) => {
                return self.make_equal(&IntermediateType::Number(id, *num), i)
            }
            (IntermediateNumber::Indeterminate, TypeSignature::Plain(p)) =>
                return Err(format!("Not implemented: {{IndeterminateNumber := Plain{}}}", p)),
            (IntermediateNumber::Float, TypeSignature::Plain(p)) =>
                return Err(format!("Not implemented: {{IndeterminateFloat := Plain{}}}", p)),
            (_, t @ TypeSignature::Struct(..))
            | (_, t @ TypeSignature::Enum(..))
            | (_, t @ TypeSignature::Tuple(..))
            | (_, t @ TypeSignature::Array(..))
            | (_, t @ TypeSignature::DynamicArray(..))
            | (_, t @ TypeSignature::Function(..)) => return Err(format!("Cannot use {:?} like a number", t)),
            (IntermediateNumber::Indeterminate, TypeSignature::Primitive(t))
            | (IntermediateNumber::Float, TypeSignature::Primitive(t @ Primitive::Float))
            | (IntermediateNumber::Float, TypeSignature::Primitive(t @ Primitive::Double))
            | (IntermediateNumber::Double, TypeSignature::Primitive(t @ Primitive::Double)) => {
                TypeSignature::Primitive(*t)
            }
            (IntermediateNumber::Indeterminate, TypeSignature::Vector(t))
            | (IntermediateNumber::Float, TypeSignature::Vector(t @ Primitive::Float))
            | (IntermediateNumber::Float, TypeSignature::Vector(t @ Primitive::Double))
            | (IntermediateNumber::Double, TypeSignature::Vector(t @ Primitive::Double)) 
            | (IntermediateNumber::Indeterminate, TypeSignature::Attribute(box TypeSignature::Vector(t), _))
            | (IntermediateNumber::Float, TypeSignature::Attribute(box TypeSignature::Vector(t @ Primitive::Float), _))
            | (IntermediateNumber::Float, TypeSignature::Attribute(box TypeSignature::Vector(t @ Primitive::Double), _))
            | (IntermediateNumber::Double, TypeSignature::Attribute(box TypeSignature::Vector(t @ Primitive::Double), _)) => {
                TypeSignature::Vector(*t)
            }
            (IntermediateNumber::Indeterminate, TypeSignature::Pointer {..})
                // Cast pointer to int, eg. if ptr { ptr is not null } else { ptr is null }
                => return Ok(()),
            // XXX: temporory hack for C
            (IntermediateNumber::Float, TypeSignature::Primitive(..)) => return Ok(()),
            otherwise => return not_impl!(otherwise),
        };

        self.infer_map
            .insert(id, IntermediateType::Exact(Box::new(res_ty)));
        Ok(())
    }

    fn make_equal_num(
        &mut self,
        id_1: i128,
        num_1: &IntermediateNumber,
        id_2: i128,
        num_2: &IntermediateNumber,
    ) -> Result<(), String> {
        if let Some(ty) = self.infer_map.get(&id_1).cloned() {
            return self.make_equal(&ty, &IntermediateType::Number(id_2, *num_2));
        } else if let Some(ty) = self.infer_map.get(&id_2).cloned() {
            return self.make_equal(&IntermediateType::Number(id_1, *num_1), &ty);
        }
        match (num_1, num_2) {
            (IntermediateNumber::Indeterminate, t) => {
                self.add_type_alias(id_1, IntermediateType::Number(id_2, *t))
            }
            (t, IntermediateNumber::Indeterminate) => {
                self.add_type_alias(id_2, IntermediateType::Number(id_1, *t))
            }
            (IntermediateNumber::Float, IntermediateNumber::Float) => Ok(()),
            (IntermediateNumber::Double, IntermediateNumber::Double) => Ok(()),
            (IntermediateNumber::Float, IntermediateNumber::Double) => self.add_type_alias(
                id_1,
                IntermediateType::new_number(IntermediateNumber::Double),
            ),
            (IntermediateNumber::Double, IntermediateNumber::Float) => self.add_type_alias(
                id_2,
                IntermediateType::new_number(IntermediateNumber::Double),
            ),
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }

    fn make_equal_to_num(&mut self, ty: &IntermediateType) -> Result<IntermediateType, String> {
        let num = self.get_number_ty();
        self.make_equal(&ty, &num)?;
        Ok(num)
    }

    fn unify_types(
        &mut self,
        lvalue: &TypeSignature,
        rvalue: &TypeSignature,
    ) -> Result<(), String> {
        if lvalue == rvalue {
            return Ok(());
        }

        if let TypeSignature::Infer(infer) = rvalue {
            return self.unify_with_infer(infer, lvalue);
        }

        if let TypeSignature::Plain(name) = rvalue {
            if let Some(ty) = self.get_ident_ty(name) {
                if &ty != rvalue {
                    return self.unify_types(&lvalue, &ty);
                }
            }
        }

        if let TypeSignature::Pointer {
            ty: box TypeSignature::Primitive(Primitive::Void),
            ..
        } = lvalue
        {
            return Ok(());
        } else if let TypeSignature::Pointer {
            ty: box TypeSignature::Primitive(Primitive::Void),
            ..
        } = rvalue
        {
            return Ok(());
        }

        match lvalue {
            TypeSignature::Plain(name) => {
                if let Some(ty) = self.get_ident_ty(name) {
                    self.unify_types(&ty, rvalue)
                } else {
                    self.fail_if(lvalue != rvalue, lvalue, rvalue)
                }
            }
            TypeSignature::Primitive(prim)
            | TypeSignature::Vector(prim)
            | TypeSignature::Attribute(box TypeSignature::Vector(prim), _)
            | TypeSignature::Complex(prim) => {
                if let TypeSignature::Primitive(r_prim)
                | TypeSignature::Vector(r_prim)
                | TypeSignature::Attribute(box TypeSignature::Vector(r_prim), _)
                | TypeSignature::Complex(r_prim) = rvalue
                {
                    self.unify_primitives(prim, r_prim)
                } else {
                    self.fail_if(lvalue != rvalue, lvalue, rvalue)
                }
            }
            TypeSignature::Pointer {
                ty: left_ty,
                nullable: left_nullable,
            } => match rvalue {
                TypeSignature::Pointer {
                    ty: right_ty,
                    nullable: right_nullable,
                } => {
                    if *right_nullable && !*left_nullable {
                        return Err(
                            "Cannot assign nullable pointer to a non-nullable pointer".to_string()
                        );
                    }
                    self.unify_types(left_ty, right_ty)
                }
                TypeSignature::Array(right_ty, _) | TypeSignature::DynamicArray(right_ty, _) => {
                    self.unify_types(left_ty, right_ty)
                }
                TypeSignature::Primitive(Primitive::Int(32)) => Ok(()), //XXX: not ok
                _ => not_impl!(rvalue, lvalue),
            },
            TypeSignature::Struct(left_name, left_fields) => match rvalue {
                TypeSignature::Struct(right_name, right_fields) => {
                    assert_eq!(left_name, right_name);
                    assert_eq!(left_fields, right_fields);
                    Ok(())
                }
                TypeSignature::Plain(name) => match left_name.as_ref() {
                    Some(s) if s == name => Ok(()),
                    Some(s) => Err(format!("Cannot assign {} to {}", name, s)),
                    None => Err(format!("Cannot assign {} to anonymous struct", name)),
                },
                otherwise => panic!("Not implemented: {:?}", otherwise),
            },
            TypeSignature::Enum(_name, _fields) => not_impl!(lvalue, rvalue),
            TypeSignature::Union(_name, _fields) => not_impl!(lvalue, rvalue),
            TypeSignature::Tuple(_tys) => not_impl!(lvalue, rvalue),
            TypeSignature::Array(l_ty, _) => match rvalue {
                TypeSignature::Array(r_ty, _) => self.unify_types(&**l_ty, &**r_ty),
                TypeSignature::DynamicArray(r_ty, _) => self.unify_types(&**l_ty, &**r_ty),
                _ => not_impl!(lvalue, rvalue),
            },
            TypeSignature::DynamicArray(ty, _expr) => {
                let right = rvalue.dereference(&self.infer_map).unwrap();
                self.unify_types(&ty, &right)
            }
            TypeSignature::Function(left_params, left_return_value) => match rvalue {
                TypeSignature::Function(right_params, right_return_value) => {
                    for (left, right) in left_params.iter().zip(right_params.iter()) {
                        self.unify_types(left.ty_field(), right.ty_field())?;
                    }
                    self.unify_types(left_return_value, right_return_value)
                }
                _otherwise => not_impl!(lvalue, rvalue),
            },
            TypeSignature::Infer(infer) => self.unify_with_infer(infer, rvalue),
            TypeSignature::Attribute(ty, attrs) => not_impl!(ty, attrs)
        }
    }

    fn unify_primitives(&mut self, lvalue: &Primitive, rvalue: &Primitive) -> Result<(), String> {
        match (lvalue, rvalue) {
            (Primitive::Int(l), Primitive::Int(r)) if l >= r => Ok(()),
            (Primitive::UInt(l), Primitive::UInt(r)) if l >= r => Ok(()),

            // Allow placing smaller uints to larger ints
            //(Primitive::Int(l), Primitive::UInt(r)) if l > r => Ok(()),

            // XXX: C allows this, so allow this for now
            (Primitive::Int(l), Primitive::UInt(r)) if l == r => Ok(()),
            (Primitive::UInt(l), Primitive::Int(r)) if l == r => Ok(()),

            (l, r) => self.fail_if_prim(l != r, l, r),
        }
    }

    fn fail_if(
        &self,
        cond: bool,
        left: &TypeSignature,
        right: &TypeSignature,
    ) -> Result<(), String> {
        if cond {
            Err(format!("Cannot assign {:?} to {:?}", right, left))
        } else {
            Ok(())
        }
    }

    fn fail_if_prim(
        &self,
        _cond: bool,
        _left: &Primitive,
        _right: &Primitive,
    ) -> Result<(), String> {
        // XXX: hack for C
        //if cond {
        //    Err(format!("Cannot implicitly assign {} to {}", right, left))
        //} else {
        Ok(())
        //}
    }
}

struct TypeInserter<'a> {
    infer_map: &'a mut HashMap<i128, IntermediateType>,
    current_statement: String,
}

impl ASTVisitor for TypeInserter<'_> {
    unit_result!();
    type Err = String;
    fn visit_ty(&mut self, s: &mut TypeSignature) -> Result<(), String> {
        match s.clone() {
            TypeSignature::Infer(IntermediateType::Any(id)) => {
                if let Some(ty) = self.infer_map.get(&id).cloned() {
                    *s = From::from(ty);
                    self.visit_ty(s)?;
                } else {
                    return Err(format!("Did not find type for id {}", id));
                }
            }
            TypeSignature::Infer(IntermediateType::Number(id, more)) => {
                if let Some(ty) = self.infer_map.get(&id).cloned() {
                    *s = From::from(ty);
                    self.visit_ty(s)?;
                } else {
                    match more {
                        _ => *s = TypeSignature::Primitive(Primitive::Int(32)),
                    }
                }
            }
            _ => {}
        }
        walk_ty(self, s)
    }

    fn visit_statement(&mut self, s: &mut Statement) -> Result<(), String> {
        self.current_statement = format!("{:?}", s);
        walk_statement(self, s)
    }
}
