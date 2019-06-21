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
        From::from(IntermediateType::Exact(Box::new(TypeSignature::Function(
            vec![param(&ulong)],
            Box::new(void_ptr.clone()),
        )))),
    );
    map.insert(
        From::from("write"),
        From::from(IntermediateType::Exact(Box::new(TypeSignature::Function(
            vec![param(&int), param(&void_ptr), param(&ulong)],
            Box::new(ulong.clone()),
        )))),
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
        dbg!(&s);

        println!("Inferring types");
        self.visit_s(s);
        dbg!(&s);
        dbg!(&self.infer_map);

        println!("Inserting types");
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
                let ty = self.get_type(Some(name.clone()), e);
                let intermediate: IntermediateType = From::from(*exact_ty.clone());
                self.make_equal(&ty, &intermediate);
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
                return From::from(ty.clone());
            }
        }
        panic!("Unknown identifier: {}", t);
    }

    fn get_number_ty(&mut self) -> IntermediateType {
        self.ty_index -= 1;
        IntermediateType::Number(self.ty_index)
    }

    fn get_type(&mut self, name: Option<Rc<str>>, expression: &Expression) -> IntermediateType {
        let ty = match expression {
            Expression::PrimaryExpression(expr) => self.get_primary_expr_type(expr),
            Expression::Op(Token::Operator(_, t), ExprList::List(list)) => match t.as_ref() {
                "+" | "-" | "*" | "%" => {
                    assert_eq!(list.len(), 2);
                    let left = self.get_type(None, &list[0]);
                    let right = self.get_type(None, &list[1]);
                    let num = self.get_number_ty();
                    self.make_equal(&left, &num);
                    self.make_equal(&left, &right);
                    left
                }
                "&" => {
                    assert_eq!(list.len(), 1);

                    IntermediateType::Exact(Box::new(TypeSignature::Pointer {
                        ty: Box::new(TypeSignature::Infer(self.get_type(None, &list[0]))),
                        nullable: false,
                    }))
                }
                otherwise => panic!("Not implemented: {:?}", otherwise),
            },
            otherwise => panic!("Not implemented: {:?}", otherwise),
        };
        if let Some(n) = name {
            self.push_type(n, ty.clone());
        }
        ty
    }

    fn get_primary_expr_type(&mut self, expr: &PrimaryExpression) -> IntermediateType {
        match expr {
            PrimaryExpression::Identifier(t) => From::from(self.get_ident_ty(t.as_ref())),
            PrimaryExpression::Literal(Literal::Integer(..)) => {
                From::from(TypeSignature::Plain(From::from("int")))
            }
            PrimaryExpression::ArrayAccess(expr, _index) => {
                match self.get_primary_expr_type(expr) {
                    IntermediateType::Exact(expr_ty) => match *expr_ty {
                        TypeSignature::Tuple(list) => {
                            let lit = expr
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
                        | TypeSignature::DynamicArray(ty, _) => From::from(*ty),
                        otherwise => panic!("Not implemented: {:?}", otherwise),
                    },

                    otherwise => panic!("Not implemented: {:?}", otherwise),
                }
            }
            PrimaryExpression::Lambda(Lambda::Lambda(params, return_type, block)) => {
                if let TypeSignature::Infer(_id) = return_type {
                    self.push_block();
                    for LambdaParam::LambdaParam(name, ty) in params {
                        self.push_type(name.clone(), From::from(*ty.clone()));
                    }
                    let block_ty = self.get_block_expr_type(&*block);
                    let exprs = get_return_expressions(block);
                    dbg!(&exprs);
                    if let Some((last, rest)) = exprs.split_last() {
                        let left = self.get_type(None, last);
                        dbg!(&left);
                        for expr in rest {
                            let right = self.get_type(None, expr);
                            self.make_equal(&right, &left);
                        }
                    }
                    let ret_ty = if let Some(ty) = block_ty {
                        if let Some(ret_expr) = exprs.get(0) {
                            let left = self.get_type(None, ret_expr);
                            self.make_equal(&ty, &left);
                        }
                        ty
                    } else if let Some(expr) = exprs.get(0) {
                        self.get_type(None, expr)
                    } else {
                        From::from(TypeSignature::Plain(From::from("void")))
                    };
                    self.make_equal(&From::from(return_type.clone()), &ret_ty);
                    let ty = From::from(TypeSignature::Function(
                        params.iter().cloned().map(From::from).collect(),
                        Box::new(From::from(ret_ty)),
                    ));
                    self.pop_block();
                    ty
                } else {
                    From::from(TypeSignature::Function(
                        params.iter().cloned().map(From::from).collect(),
                        Box::new(return_type.clone()),
                    ))
                }
            }
            PrimaryExpression::Call(name, _args) => match From::from(self.get_ident_ty(name)) {
                TypeSignature::Function(_, return_type) => From::from(*return_type),
                //TypeSignature::Infer(inferred) => self.call_inferred(&inferred, args),
                otherwise => panic!("Not implemented: {:?}", otherwise),
            },
            PrimaryExpression::Expression(expr) => self.get_type(None, expr),
            PrimaryExpression::StructInitialization(ident, _) => {
                From::from(TypeSignature::Plain(ident.clone()))
            }
            otherwise => panic!("Not implemented: {:?}", otherwise),
        }
    }

    fn get_block_expr_type(&mut self, expr: &BlockExpression) -> Option<IntermediateType> {
        match expr {
            BlockExpression::Block(block) => self.get_block_type(&*block),
            BlockExpression::If(arms, otherwise) => {
                if let Some(otherwise) = otherwise {
                    arms.iter()
                        .map(|(expr, b)| {
                            let num_ty = self.get_number_ty();
                            let expr_ty = self.get_type(None, expr);
                            self.make_equal(&num_ty, &expr_ty);
                            self.get_block_type(b)
                        })
                        .collect::<Vec<_>>()
                        .iter()
                        .fold(self.get_block_type(otherwise), |prev, this| {
                            assert_eq!(prev, *this);
                            prev
                        })
                } else {
                    None
                }
            }
            BlockExpression::While(..) | BlockExpression::For(..) => None,
        }
    }

    fn get_block_type(&mut self, expr: &Block) -> Option<IntermediateType> {
        match expr {
            Block::Statements(stmts) => {
                stmts.iter().flat_map(|e| self.get_statement_type(e)).last()
            }
        }
    }

    fn get_statement_type(&mut self, stmt: &Statement) -> Option<IntermediateType> {
        match stmt {
            Statement::Declaration(decl) => {
                self.visit_declaration(&mut decl.clone());
                None
            }
            Statement::BlockExpression(block) => self.get_block_expr_type(&**block),
            Statement::Expression(expr) => {
                self.visit_expression(&mut expr.clone());
                None
            }
            Statement::Return(_expr) => None,
        }
    }

    /*
     * Assignable:
     * a <- a
     * fn(v_0, v_1...) <- fn(t_0, t_1...) iff v_0 <- t_0 ...
     *
     * Unification:
     * a
     *
     *  a => id
     *  a <> b => forall b => b = a
     */

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

            (IntermediateType::Number(..), IntermediateType::Number(..)) => {}

            (left, right) => panic!("Not implemented: {:?} {:?}", left, right),
        }
    }

    fn unify_types(&mut self, lvalue: &TypeSignature, rvalue: &TypeSignature) {
        match lvalue {
            TypeSignature::Plain(_name) => self.fail_if(lvalue != rvalue, lvalue, rvalue),
            TypeSignature::Pointer { nullable: _, ty: _ } => unimplemented!(),
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

struct ReturnExpressions {
    expressions: Vec<Expression>,
}

fn get_return_expressions(block: &BlockExpression) -> Vec<Expression> {
    let mut exprs = ReturnExpressions {
        expressions: Vec::new(),
    };
    exprs.visit_block_expression(&mut block.clone());
    exprs.expressions
}

impl ASTVisitor for ReturnExpressions {
    fn visit_statement(&mut self, s: &mut Statement) {
        match s.clone() {
            Statement::Return(expr) => {
                expr.map(|e| self.expressions.push(*e));
            }
            _ => {}
        }
        walk_statement(self, s);
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
            TypeSignature::Infer(IntermediateType::Number(_)) => {
                *s = TypeSignature::Plain(From::from("int"));
            }
            _ => {}
        }
        walk_ty(self, s);
    }
}
