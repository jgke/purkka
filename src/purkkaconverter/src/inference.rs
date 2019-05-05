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
    scope: Vec<HashMap<Rc<str>, TypeSignature>>
}

fn get_std_lib() -> HashMap<Rc<str>, TypeSignature> {
    let mut map = HashMap::new();

    fn param(ty: &TypeSignature) -> Param {
        Param::Anon(Box::new(ty.clone()))
    }

    let void = TypeSignature::Plain(From::from("void"));
    let ulong = TypeSignature::Plain(From::from("ulong"));
    let int = TypeSignature::Plain(From::from("int"));
    let void_ptr = TypeSignature::Pointer { ty: Box::new(void), nullable: true };

    map.insert(From::from("malloc"), TypeSignature::Function(vec![param(&ulong)], Box::new(void_ptr.clone())));
    map.insert(From::from("write"), TypeSignature::Function(vec![param(&int), param(&void_ptr), param(&ulong)], Box::new(ulong.clone())));

    map
}

impl<'a> TreeTransformer<'a> for TypeInferrer {
    fn new(_context: &'a mut Context) -> TypeInferrer {
        TypeInferrer { scope: vec![get_std_lib()] }
    }
    fn transform(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for TypeInferrer {
    fn visit_declaration(&mut self, tree: &mut Declaration) {
        match tree {
            Declaration::Declaration(visibility, mutability, name, None, Some(e)) => {
                let ty = Box::new(self.get_type(Some(name.clone()), e));
                *tree = Declaration::Declaration(*visibility, *mutability, name.clone(), Some(ty), Some(e.clone()));
            }
            Declaration::Declaration(_, _, _, Some(_), _) => {}
            Declaration::Declaration(_, _, _, None, None) => {
                panic!("Proper type inference not yet implemented");
            }

        }
        walk_declaration(self, tree);
    }

    fn visit_lambda(&mut self, tree: &mut Lambda) {
        self.push_block();
        let Lambda::Lambda(params, _, e) = tree;
        for param in params {
            match param {
                Param::Anon(_) => panic!("Inferred types for parameters is not supported"),
                Param::Param(name, ty) => self.push_type(name.clone(), *ty.clone())
            }
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
    fn get_type(&mut self, name: Option<Rc<str>>, expression: &Expression) -> TypeSignature {
        let ty = match expression {
            Expression::PrimaryExpression(expr) => self.get_primary_expr_type(expr),
            Expression::Op(Token::Operator(_, t), ExprList::List(list)) => {
                match t.as_ref() {
                    "+" | "-" | "*" | "%" => self.get_type(None, &list[0]),
                    "&" => TypeSignature::Pointer { ty: Box::new(self.get_type(None, &list[0])), nullable: false },
                    otherwise => panic!("Not implemented: {:?}", otherwise)
                }
            }
            otherwise => panic!("Not implemented: {:?}", otherwise)
        };
        for n in name {
            self.push_type(n, ty.clone());
        }
        ty
    }

    fn get_primary_expr_type(&mut self, expr: &PrimaryExpression) -> TypeSignature {
        match expr {
            PrimaryExpression::Identifier(t) => self.get_ident_ty(t.as_ref()),
            PrimaryExpression::Literal(Literal::Integer(..)) => TypeSignature::Plain(From::from("int")),
            PrimaryExpression::ArrayAccess(expr, _index) => {
                match self.get_primary_expr_type(expr) {
                    TypeSignature::Tuple(list) => {
                        let lit = expr.eval(&HashMap::new()).expect("Tuple index must be a constant expression");
                        let index = match lit {
                            Literal::Integer(Token::Integer(_, i)) => i,
                            otherwise => panic!("Tuple index must be a integer constant expression (got {:?})", otherwise)
                        };
                        list[usize::try_from(index).unwrap()].clone()
                    }
                    TypeSignature::Pointer { ty, .. } | TypeSignature::Array(ty, _) | TypeSignature::DynamicArray(ty, _) =>
                        *ty.clone(),
                    otherwise => panic!("Not implemented: {:?}", otherwise)
                }
            }
            PrimaryExpression::Lambda(Lambda::Lambda(params, return_type, block)) => {
                if let  TypeSignature::Infer = return_type {
                    TypeSignature::Function(params.to_vec(), Box::new(self.get_block_expr_type(&*block)))
                } else {
                    TypeSignature::Function(params.to_vec(), Box::new(return_type.clone()))
                }
            }
            PrimaryExpression::Call(name, _args) => {
                match self.get_ident_ty(name) {
                    TypeSignature::Function(_, return_type) => *return_type.clone(),
                    otherwise => panic!("Not implemented: {:?}", otherwise)
                }
            }
            PrimaryExpression::Expression(expr) => self.get_type(None, expr),
            PrimaryExpression::StructInitialization(ident, _) => TypeSignature::Plain(ident.clone()),
            otherwise => panic!("Not implemented: {:?}", otherwise)
        }
    }

    fn get_ident_ty(&self, t: &str) -> TypeSignature {
        for tys in self.scope.iter().rev() {
            if let Some(ty) = tys.get(t) {
                return ty.clone();
            }
        }
        panic!("Unknown identifier: {}", t);
    }

    fn get_block_expr_type(&mut self, expr: &BlockExpression) -> TypeSignature {
        match expr {
            BlockExpression::Block(block) => self.get_block_type(&*block),
            BlockExpression::If(arms, otherwise) => {
                if let Some(otherwise) = otherwise {
                    arms.iter()
                        .map(|(_, b)| self.get_block_type(b))
                        .collect::<Vec<_>>().iter()
                        .fold(self.get_block_type(otherwise),
                            |prev, this| {
                            assert_eq!(prev, *this);
                            prev
                        })
                } else {
                    TypeSignature::Plain(From::from("void"))
                }
            }
            BlockExpression::While(..) | BlockExpression::For(..) =>
                    TypeSignature::Plain(From::from("void"))
        }
    }

    fn get_block_type(&mut self, expr: &Block) -> TypeSignature {
        match expr {
            Block::Statements(stmts) => {
                let tys = stmts.iter()
                    .flat_map(|e| self.get_statement_type(e))
                    .collect::<Vec<TypeSignature>>();
                if tys.is_empty() {
                    TypeSignature::Plain(From::from("void"))
                } else {
                    tys.iter()
                        .fold(tys[0].clone(),
                            |prev, this| {
                            if prev != *this {
                                TypeSignature::Plain(From::from("void"))
                            } else {
                                prev
                            }
                        })
                }
            }
        }
    }

    fn get_statement_type(&mut self, stmt: &Statement) -> Option<TypeSignature> {
        match stmt {
            Statement::Declaration(decl) => {
                self.visit_declaration(&mut decl.clone());
                None
            }
            Statement::BlockExpression(block) => Some(self.get_block_expr_type(&**block)),
            Statement::Expression(expr) => {
                self.visit_expression(&mut expr.clone());
                None
            }
            Statement::Return(expr) => expr.as_ref().map(|e| self.get_type(None, &**e)),
        }
    }

    fn push_block(&mut self) {
        self.scope.push(HashMap::new());
    }

    fn push_type(&mut self, name: Rc<str>, ty: TypeSignature) {
        self.scope.last_mut().unwrap().insert(name, ty);
    }

    fn pop_block(&mut self) {
        self.scope.pop();
    }
}
