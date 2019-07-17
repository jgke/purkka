#![feature(box_patterns, drain_filter)]

use std::convert::TryFrom;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::rc::Rc;

use cparser::grammar as cp;
use ctoken::token as ct;
use purkkaparser::parser::{Operators, Symbols};
use purkkasyntax as pp;
use purkkasyntax::{Primitive, TypeSignature};
use purkkatoken::token as pt;
use resolve::Declarations;

pub mod traits;

mod array;
mod declarations;
mod imports;
pub mod inference;
mod lambda;
mod operator;
mod typedef;

use traits::TreeTransformer;

use array::ArrayToPointer;
use declarations::FetchDeclarations;
use imports::StripImports;
use inference::TypeInferrer;
use lambda::StripLambda;
use operator::InlineOperators;
use typedef::InlineTypedef;

#[derive(Clone, Debug)]
pub struct PurkkaToC {
    pub functions: Vec<(Rc<str>, pp::Lambda, bool)>,
    pub global_includes: HashSet<Rc<str>>,
    pub local_includes: HashSet<Rc<str>>,
    pub operators: Operators,
    pub symbols: Symbols,
}

pub fn transform(purkka_tree: &mut pp::S, operators: Operators, symbols: Symbols) -> PurkkaToC {
    let mut context = PurkkaToC::new(operators, symbols);
    InlineTypedef::new(&mut context).transform(purkka_tree);
    InlineOperators::new(&mut context).transform(purkka_tree);
    TypeInferrer::new(&mut context).transform(purkka_tree);
    ArrayToPointer::new(&mut context).transform(purkka_tree);
    StripLambda::new(&mut context).transform(purkka_tree);
    StripImports::new(&mut context).transform(purkka_tree);
    context
}

pub fn convert(mut purkka_tree: pp::S, operators: Operators, symbols: Symbols) -> (cp::S, PurkkaToC) {
    let mut context = transform(&mut purkka_tree, operators, symbols);
    (context.s(purkka_tree), context)
}

pub fn fetch_identifiers_from_prk(tree: &mut pp::S) -> HashMap<Rc<str>, pp::Declaration> {
    let mut decls = FetchDeclarations::new();
    decls.fetch_declarations(tree);
    decls
        .declarations
        .into_iter()
        .map(|decl| {
            let pp::Declaration::Declaration(_, _, _, name, _, _) = &decl;
            (name.clone(), decl)
        })
        .collect()
}

pub fn fetch_identifiers_from_c(_tree: &mut cp::S) -> HashMap<Rc<str>, pp::Declaration> {
    panic!()
}

impl PurkkaToC {
    fn new(operators: Operators, symbols: Symbols) -> PurkkaToC {
        PurkkaToC {
            functions: Vec::new(),
            global_includes: HashSet::new(),
            local_includes: HashSet::new(),
            operators,
            symbols,
        }
    }

    fn push_function(&mut self, name: Rc<str>, lambda: pp::Lambda, inline: bool) {
        self.functions.push((name, lambda, inline));
    }

    fn push_anonymous_function(&mut self, lambda: pp::Lambda) -> Rc<str> {
        let name: Rc<str> = From::from(format!("_lambda_{}", self.functions.len()));
        self.functions.push((name.clone(), lambda, true));
        name
    }

    /* Output the C parse tree */

    pub fn s(&mut self, purkka_tree: pp::S) -> cp::S {
        match purkka_tree {
            pp::S::TranslationUnit(u) => cp::S::TranslationUnit(self.translation_unit(u)),
        }
    }

    pub fn translation_unit(&mut self, k: pp::TranslationUnit) -> cp::TranslationUnit {
        match k {
            pp::TranslationUnit::Units(u) => {
                let tys = self
                    .symbols
                    .types
                    .clone()
                    .into_iter()
                    .map(|ty| self.format_type_as_external_decl(ty.1))
                    .collect::<Vec<_>>();
                let funcs = self
                    .functions
                    .drain(..)
                    .collect::<Vec<_>>()
                    .into_iter()
                    .map(|(n, u, i)| self.format_lambda_as_external_decl(n.clone(), u.clone(), i))
                    .collect::<Vec<_>>();
                cp::TranslationUnit::Units(
                    tys.into_iter()
                        .chain(funcs.into_iter())
                        .chain(u.into_iter().map(|u| self.unit_to_external_decl(u)))
                        .collect(),
                )
            }
        }
    }

    fn format_lambda_as_external_decl(
        &mut self,
        name: Rc<str>,
        pp::Lambda::Lambda(params, ty, block): pp::Lambda,
        inline: bool,
    ) -> cp::ExternalDeclaration {
        use cp::DeclarationSpecifiers::DeclarationSpecifiers as DSpec;
        let DSpec(mut specs, c_ty) = self.type_to_declaration_specifiers(ty.clone());
        if let Some(ref mut specs) = specs {
            specs.0.inline = inline;
        } else if inline {
            let mut s = cp::Specifiers::default(); 
            s.0.inline = true;
            specs = Some(s);
        }
        let params = self.function_params_from_params(name.clone(), params);
        let statements = self.block_to_statement_list(block);
        cp::ExternalDeclaration::FunctionDefinition(Box::new(
            cp::FunctionDefinition::FunctionDefinition(
                Some(Box::new(DSpec(specs, c_ty))),
                Box::new(cp::Declarator::Declarator(self.ty_to_pointer(ty), Box::new(params))),
                Box::new(cp::CompoundStatement::Statements(statements)),
            ),
        ))
    }

    fn format_type_as_external_decl(&mut self, ty: pp::TypeSignature) -> cp::ExternalDeclaration {
        cp::ExternalDeclaration::Declaration(Box::new(cp::Declaration::Declaration(
            Box::new(self.type_to_declaration_specifiers(ty.clone())),
            vec![],
            None,
        )))
    }

    pub fn cond_and_block_to_selection_statement(
        &mut self,
        cond: pp::Expression,
        block: pp::Block,
        otherwise: Option<Box<cp::Statement>>,
    ) -> cp::SelectionStatement {
        cp::SelectionStatement::If(
            Box::new(self.expression(cond)),
            cp::Statement::CompoundStatement(Box::new(cp::CompoundStatement::Statements(
                self.block_to_statement_list(block),
            ))),
            otherwise,
        )
    }

    pub fn block_expression_to_compound_statement(
        &mut self,
        block: pp::BlockExpression,
    ) -> cp::CompoundStatement {
        match block {
            pp::BlockExpression::Block(block) => {
                cp::CompoundStatement::Statements(self.block_to_statement_list(block))
            }
            _ => cp::CompoundStatement::Statements(vec![cp::StatementOrDeclaration::Statement(
                self.block_expression_to_statement(block),
            )]),
        }
    }

    pub fn block_expression_to_statement(&mut self, block: pp::BlockExpression) -> cp::Statement {
        match block {
            pp::BlockExpression::If(arms, otherwise) => {
                let mut iter = arms.into_iter();
                let first = iter.next().unwrap();

                let else_block = otherwise
                    .map(|block| {
                        Some(cp::Statement::CompoundStatement(Box::new(
                            cp::CompoundStatement::Statements(self.block_to_statement_list(*block)),
                        )))
                    })
                    .unwrap_or(None);

                let tail = iter.rev().fold(else_block, |prev, next| {
                    Some(cp::Statement::SelectionStatement(Box::new(
                        self.cond_and_block_to_selection_statement(
                            *next.0,
                            *next.1,
                            prev.map(Box::new),
                        ),
                    )))
                });

                cp::Statement::SelectionStatement(Box::new(
                    self.cond_and_block_to_selection_statement(
                        *first.0,
                        *first.1,
                        tail.map(Box::new),
                    ),
                ))
            }
            pp::BlockExpression::Block(block) => cp::Statement::CompoundStatement(Box::new(
                cp::CompoundStatement::Statements(self.block_to_statement_list(block)),
            )),
            pp::BlockExpression::For(first, second, third, block, None) => {
                cp::Statement::IterationStatement(Box::new(cp::IterationStatement::For(
                    ct::Token::For(0),
                    ct::Token::OpenParen(0),
                    self.expressions_to_for_expr(
                        first.map(|t| *t),
                        second.map(|t| *t),
                        third.map(|t| *t),
                    ),
                    ct::Token::CloseParen(0),
                    Box::new(self.block_to_statement(*block)),
                )))
            }
            pp::BlockExpression::While(expr, block, None) => {
                cp::Statement::IterationStatement(Box::new(cp::IterationStatement::While(
                    ct::Token::While(0),
                    ct::Token::OpenParen(0),
                    Box::new(self.expression(*expr)),
                    ct::Token::CloseParen(0),
                    Box::new(self.block_to_statement(*block)),
                )))
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn block_to_statement_list(&mut self, block: pp::Block) -> Vec<cp::StatementOrDeclaration> {
        match block {
            pp::Block::Statements(statements) => statements
                .into_iter()
                .map(|next| self.statement_to_statement_or_declaration(next))
                .collect(),
        }
    }

    pub fn block_to_statement(&mut self, block: pp::Block) -> cp::Statement {
        cp::Statement::CompoundStatement(Box::new(cp::CompoundStatement::Statements(
            self.block_to_statement_list(block),
        )))
    }

    pub fn expressions_to_for_expr(
        &mut self,
        first: Option<pp::Statement>,
        second: Option<pp::Expression>,
        third: Option<pp::Expression>,
    ) -> cp::ForExpr {
        match third {
            Some(expr) => cp::ForExpr::ForExpr(
                self.statement_to_expression_or_declaration(first),
                Box::new(self.expression_to_expression_statement(second)),
                Box::new(self.expression(expr)),
            ),
            None => cp::ForExpr::EmptyLast(
                self.statement_to_expression_or_declaration(first),
                Box::new(self.expression_to_expression_statement(second)),
            ),
        }
    }

    pub fn statement_to_expression_statement(
        &mut self,
        expr: Option<pp::Statement>,
    ) -> cp::ExpressionStatement {
        cp::ExpressionStatement::Expression(expr.map(|e| Box::new(self.statement_to_expression(e))))
    }

    pub fn statement_to_expression_or_declaration(
        &mut self,
        expr: Option<pp::Statement>,
    ) -> cp::DeclarationOrExpression {
        match expr {
            Some(pp::Statement::Declaration(decl)) => {
                cp::DeclarationOrExpression::Declaration(Box::new(self.convert_declaration(*decl)))
            }
            expr => cp::DeclarationOrExpression::ExpressionStatement(Box::new(
                self.statement_to_expression_statement(expr),
            )),
        }
    }

    pub fn expression_to_expression_statement(
        &mut self,
        expr: Option<pp::Expression>,
    ) -> cp::ExpressionStatement {
        cp::ExpressionStatement::Expression(expr.map(|e| Box::new(self.expression(e))))
    }

    pub fn statement_to_expression(&mut self, expr: pp::Statement) -> cp::Expression {
        match expr {
            pp::Statement::Expression(e) => self.expression(*e),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn statement_to_statement_or_declaration(
        &mut self,
        statement: pp::Statement,
    ) -> cp::StatementOrDeclaration {
        match statement {
            pp::Statement::Declaration(decl) => {
                cp::StatementOrDeclaration::Declaration(self.convert_declaration(*decl))
            }
            pp::Statement::BlockExpression(block) => {
                cp::StatementOrDeclaration::Statement(self.block_expression_to_statement(*block))
            }
            pp::Statement::Expression(expr) => {
                cp::StatementOrDeclaration::Statement(cp::Statement::ExpressionStatement(Box::new(
                    cp::ExpressionStatement::Expression(Some(Box::new(self.expression(*expr)))),
                )))
            }
            pp::Statement::Return(Some(e)) => cp::StatementOrDeclaration::Statement(
                cp::Statement::JumpStatement(Box::new(cp::JumpStatement::Return(
                    ct::Token::Return(0),
                    self.expression(*e),
                    ct::Token::Semicolon(0),
                ))),
            ),
            pp::Statement::Return(None) => {
                cp::StatementOrDeclaration::Statement(cp::Statement::JumpStatement(Box::new(
                    cp::JumpStatement::ReturnVoid(ct::Token::Return(0), ct::Token::Semicolon(0)),
                )))
            }
            pp::Statement::Pragma(s) => cp::StatementOrDeclaration::Declaration(
                cp::Declaration::Pragma(cp::Pragma::Pragma(s)),
            ),
        }
    }

    pub fn function_params_from_params(
        &mut self,
        name: Rc<str>,
        params: Vec<pp::LambdaParam>,
    ) -> cp::DirectDeclarator {
        let e = Box::new(cp::DirectDeclarator::Identifier(name));
        cp::DirectDeclarator::Function(e, self.parameter_list_from_params(params))
    }

    pub fn function_pointer_from_params(
        &mut self,
        name: Rc<str>,
        params: Vec<pp::LambdaParam>,
    ) -> cp::DirectDeclarator {
        let e = Box::new(cp::DirectDeclarator::Parens(Box::new(
            cp::Declarator::Declarator(
                Some(Box::new(cp::Pointer::Ptr(
                    cp::TypeQualifiers::default(),
                    None,
                ))),
                Box::new(cp::DirectDeclarator::Identifier(name)),
            ),
        )));
        cp::DirectDeclarator::Function(e, self.parameter_list_from_params(params))
    }

    pub fn parameter_list_from_params(
        &mut self,
        params: Vec<pp::LambdaParam>,
    ) -> Vec<cp::FunctionParam> {
        params
            .into_iter()
            .map(|t| match t {
                pp::LambdaParam::LambdaParam(name, ty) => {
                    let decl_spec = self.type_to_declaration_specifiers(*ty.clone());
                    let decl = self.format_decl(name, *ty);
                    cp::FunctionParam::Parameter(cp::ParameterDeclaration::Declarator(
                        Box::new(decl_spec),
                        Box::new(decl),
                    ))
                }
                pp::LambdaParam::Variadic => cp::FunctionParam::Varargs,
            })
            .collect()
    }

    pub fn unit_to_external_decl(&mut self, k: pp::Unit) -> cp::ExternalDeclaration {
        match k {
            pp::Unit::Declaration(decl) => {
                cp::ExternalDeclaration::Declaration(Box::new(self.convert_declaration(*decl)))
            }
            pp::Unit::Typedef(ty) => match *ty {
                pp::Typedef::Alias(..) => unreachable!(),
                pp::Typedef::Struct(name, fields) => {
                    let c_ty = cp::CType::Compound(cp::CompoundType::Struct(
                        name,
                        Some(
                            fields
                                .into_iter()
                                .map(|field| self.struct_field(field))
                                .collect(),
                        ),
                    ));
                    cp::ExternalDeclaration::Declaration(Box::new(cp::Declaration::Declaration(
                        Box::new(cp::DeclarationSpecifiers::DeclarationSpecifiers(
                            None,
                            Some(c_ty),
                        )),
                        Vec::new(),
                        None,
                    )))
                }
                other => panic!("Not implemented: {:?}", other),
            },
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn struct_field(
        &mut self,
        pp::StructField::Field { name, ty, .. }: pp::StructField,
    ) -> cp::StructField {
        let c_ty = self.type_to_declaration_specifiers(*ty.clone());
        let decl = self.format_decl(name, *ty);
        (
            Box::new(c_ty),
            vec![(cp::EitherDeclarator::Declarator(decl), None)],
        )
    }

    pub fn convert_declaration(&mut self, k: pp::Declaration) -> cp::Declaration {
        match k {
            pp::Declaration::Declaration(_, _mutable, _inline, name, ty, Some(expr)) => {
                cp::Declaration::Declaration(
                    Box::new(self.type_to_declaration_specifiers(*ty.clone())),
                    vec![cp::InitDeclarator::Assign(
                        Box::new(self.format_decl(name, *ty)),
                        ct::Token::Assign(0),
                        Box::new(cp::AssignmentOrInitializerList::AssignmentExpression(
                            self.assignment_expression(*expr),
                        )),
                    )],
                    None,
                )
            }
            pp::Declaration::Declaration(_, _mutable, _inline, name, ty, None) => {
                cp::Declaration::Declaration(
                    Box::new(self.type_to_declaration_specifiers(*ty.clone())),
                    vec![cp::InitDeclarator::Declarator(Box::new(
                        self.format_decl(name, *ty),
                    ))],
                    None,
                )
            }
        }
    }

    pub fn type_to_declaration_specifiers(
        &mut self,
        ty: TypeSignature,
    ) -> cp::DeclarationSpecifiers {
        match ty {
            TypeSignature::Plain(ty) => {
                let c_ty = match ty.as_ref() {
                    "void" => cp::CType::Void,
                    "int" => cp::CType::Primitive(None, cp::PrimitiveType::Int),
                    "long" => cp::CType::Primitive(None, cp::PrimitiveType::Long),
                    "char" => cp::CType::Primitive(None, cp::PrimitiveType::Char),
                    "float" => cp::CType::Primitive(None, cp::PrimitiveType::Float),
                    "double" => cp::CType::Primitive(None, cp::PrimitiveType::Double),
                    t if self.symbols.types.contains_key(t) => {
                        return self.type_to_declaration_specifiers(self.symbols.types[t].clone())
                    }
                    t if self.symbols.imported_types.contains_key(t) => {
                        cp::CType::Custom(ty.clone())
                    }
                    //t if self.symbols.imported_types.contains_key(t) =>
                    //    return self.type_to_declaration_specifiers(self.symbols.imported_types[t].clone()),
                    other => panic!("Not implemented: {:?}", other),
                };
                cp::DeclarationSpecifiers::DeclarationSpecifiers(None, Some(c_ty))
            }
            TypeSignature::Primitive(prim) | TypeSignature::Vector(prim) => {
                let c_ty = match prim {
                    Primitive::Void => cp::CType::Void,
                    Primitive::Int(8) => cp::CType::Primitive(None, cp::PrimitiveType::Char),
                    Primitive::Int(16) => cp::CType::Primitive(None, cp::PrimitiveType::Short),
                    Primitive::Int(32) => cp::CType::Primitive(None, cp::PrimitiveType::Int),
                    Primitive::Int(64) => cp::CType::Primitive(None, cp::PrimitiveType::Long),
                    Primitive::UInt(8) => {
                        cp::CType::Primitive(Some(false), cp::PrimitiveType::Char)
                    }
                    Primitive::UInt(16) => {
                        cp::CType::Primitive(Some(false), cp::PrimitiveType::Short)
                    }
                    Primitive::UInt(32) => {
                        cp::CType::Primitive(Some(false), cp::PrimitiveType::Int)
                    }
                    Primitive::UInt(64) => {
                        cp::CType::Primitive(Some(false), cp::PrimitiveType::Long)
                    }
                    Primitive::Float => cp::CType::Primitive(None, cp::PrimitiveType::Float),
                    Primitive::Double => cp::CType::Primitive(None, cp::PrimitiveType::Double),
                    other => panic!("Not implemented: {:?}", other),
                };
                cp::DeclarationSpecifiers::DeclarationSpecifiers(None, Some(c_ty))
            }
            TypeSignature::Infer(..) => unreachable!(),
            TypeSignature::Array(ty, _) | TypeSignature::DynamicArray(ty, _) => {
                self.type_to_declaration_specifiers(*ty)
            }
            TypeSignature::Pointer { ty, .. } => self.type_to_declaration_specifiers(*ty),
            TypeSignature::Function(_params, ret_ty) => {
                self.type_to_declaration_specifiers(*ret_ty)
            }
            TypeSignature::Struct(Some(name), _) => {
                let c_ty = cp::CType::Compound(cp::CompoundType::Struct(name, None));
                cp::DeclarationSpecifiers::DeclarationSpecifiers(None, Some(c_ty))
            }
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn ty_to_pointer(&mut self, ty: TypeSignature) -> Option<Box<cp::Pointer>> {
        match ty {
            TypeSignature::Pointer { ty, .. } => Some(Box::new(cp::Pointer::Ptr(
                cp::TypeQualifiers::default(),
                self.ty_to_pointer(*ty),
            ))),
            _ => None,
        }
    }
    pub fn format_decl(&mut self, name: Rc<str>, ty: TypeSignature) -> cp::Declarator {
        cp::Declarator::Declarator(
            self.ty_to_pointer(ty.clone()),
            Box::new(self.format_direct_decl(name, ty)),
        )
    }

    pub fn format_direct_decl(&mut self, name: Rc<str>, ty: TypeSignature) -> cp::DirectDeclarator {
        match ty {
            TypeSignature::Plain(_) | TypeSignature::Primitive(_) | TypeSignature::Struct(_, _) => {
                cp::DirectDeclarator::Identifier(name)
            }
            TypeSignature::Pointer { ty, .. } => self.format_direct_decl(name, *ty),
            TypeSignature::Function(params, _ret_ty) => self
                .function_pointer_from_params(name, params.into_iter().map(From::from).collect()),
            TypeSignature::Array(ty, size) => cp::DirectDeclarator::Array(
                Box::new(self.format_direct_decl(name, *ty)),
                size.map(|lit| {
                    Box::new(self.general_expression(pp::Expression::PrimaryExpression(
                        pp::PrimaryExpression::Literal(pp::Literal::Integer(pt::Token::Integer(
                            0,
                            lit.try_into().unwrap(),
                        ))),
                    )))
                }),
            ),
            TypeSignature::DynamicArray(ty, expr) => cp::DirectDeclarator::Array(
                Box::new(self.format_direct_decl(name, *ty)),
                Some(Box::new(self.general_expression(*expr))),
            ),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    pub fn type_to_type_name(&mut self, ty: TypeSignature) -> cp::TypeName {
        cp::TypeName::TypeName(
            Box::new(self.type_to_declaration_specifiers(ty.clone())),
            Box::new(self.format_abstract_decl(ty)),
        )
    }

    pub fn format_abstract_decl(&mut self, ty: TypeSignature) -> cp::AbstractDeclarator {
        cp::AbstractDeclarator::AbstractDeclarator(
            self.ty_to_pointer(ty.clone()),
            Box::new(self.format_abstract_direct_decl(ty)),
        )
    }

    pub fn format_abstract_direct_decl(
        &mut self,
        ty: TypeSignature,
    ) -> cp::DirectAbstractDeclarator {
        match ty {
            TypeSignature::Plain(_) | TypeSignature::Primitive(_) => {
                cp::DirectAbstractDeclarator::Epsilon()
            }
            TypeSignature::Struct(_, _) => cp::DirectAbstractDeclarator::Epsilon(),
            TypeSignature::Pointer { ty, .. } => self.format_abstract_direct_decl(*ty),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    /* Expression handling */

    pub fn expression(&mut self, k: pp::Expression) -> cp::Expression {
        cp::Expression::Expression(vec![self.assignment_expression(k)])
    }

    fn is_assignment_op(&self, op: &Rc<str>) -> bool {
        match op.as_ref() {
            "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|=" => true,
            _ => false,
        }
    }

    pub fn assignment_expression(&mut self, expr: pp::Expression) -> cp::AssignmentExpression {
        match expr {
            pp::Expression::Op(ref op, ref expr_list) if self.is_assignment_op(op) => {
                let pp::ExprList::List(list) = expr_list;
                assert_eq!(list.len(), 2);
                let left = Box::new(self.unary_expression(list[0].clone()));
                let right = Box::new(self.assignment_expression(list[1].clone()));
                let tok = match op.as_ref() {
                    "=" => cp::AssignmentOperator::Assign(ct::Token::Assign(0)),
                    "*=" => cp::AssignmentOperator::TimesAssign(ct::Token::TimesAssign(0)),
                    "/=" => cp::AssignmentOperator::DivAssign(ct::Token::DivAssign(0)),
                    "%=" => cp::AssignmentOperator::ModAssign(ct::Token::ModAssign(0)),
                    "+=" => cp::AssignmentOperator::PlusAssign(ct::Token::PlusAssign(0)),
                    "-=" => cp::AssignmentOperator::MinusAssign(ct::Token::MinusAssign(0)),
                    "<<=" => {
                        cp::AssignmentOperator::BitShiftLeftAssign(ct::Token::BitShiftLeftAssign(0))
                    }
                    ">>=" => cp::AssignmentOperator::BitShiftRightAssign(
                        ct::Token::BitShiftRightAssign(0),
                    ),
                    "&=" => cp::AssignmentOperator::BitAndAssign(ct::Token::BitAndAssign(0)),
                    "^=" => cp::AssignmentOperator::BitXorAssign(ct::Token::BitXorAssign(0)),
                    "|=" => cp::AssignmentOperator::BitOrAssign(ct::Token::BitOrAssign(0)),

                    _ => unreachable!(),
                };
                cp::AssignmentExpression::Assignment(left, tok, right)
            }
            _ => cp::AssignmentExpression::TernaryExpression(self.ternary_expression(expr)),
        }
    }

    pub fn ternary_expression(&mut self, k: pp::Expression) -> cp::TernaryExpression {
        match k {
            pp::Expression::Op(ref op, ref list) if op.as_ref() == "?" => {
                let pp::ExprList::List(list) = list;
                let cond = self.general_expression(list[0].clone());
                let if_t = self.expression(list[1].clone());
                let if_f = self.ternary_expression(list[2].clone());
                cp::TernaryExpression::Ternary(
                    cond,
                    ct::Token::Ternary(0),
                    Box::new(if_t),
                    ct::Token::Colon(0),
                    Box::new(if_f),
                )
            }
            _ => cp::TernaryExpression::GeneralExpression(self.general_expression(k)),
        }
    }

    fn is_general_op(&self, op: &Rc<str>) -> bool {
        match op.as_ref() {
            "||" | "&&" | "^" | "|" | "&" | "!=" | "==" | ">=" | "<=" | ">" | "<" | "<<" | ">>"
            | "-" | "+" | "%" | "/" | "*" => true,
            _ => false,
        }
    }

    pub fn general_expression(&mut self, k: pp::Expression) -> cp::GeneralExpression {
        match k {
            pp::Expression::Op(ref op, ref expr_list) if self.is_general_op(op) => {
                let pp::ExprList::List(list) = expr_list;
                assert_eq!(list.len(), 2);
                let left = Box::new(self.general_expression(list[0].clone()));
                let right = Box::new(self.general_expression(list[1].clone()));
                match op.as_ref() {
                    "*" => cp::GeneralExpression::Times(left, ct::Token::Times(0), right),
                    "/" => cp::GeneralExpression::Divide(left, ct::Token::Divide(0), right),
                    "%" => cp::GeneralExpression::Mod(left, ct::Token::Mod(0), right),
                    "+" => cp::GeneralExpression::Plus(left, ct::Token::Plus(0), right),
                    "-" => cp::GeneralExpression::Minus(left, ct::Token::Minus(0), right),
                    "<<" => {
                        cp::GeneralExpression::BitShiftLeft(left, ct::Token::BitShiftLeft(0), right)
                    }
                    ">>" => cp::GeneralExpression::BitShiftRight(
                        left,
                        ct::Token::BitShiftRight(0),
                        right,
                    ),
                    "<" => cp::GeneralExpression::LessThan(left, ct::Token::LessThan(0), right),
                    ">" => cp::GeneralExpression::MoreThan(left, ct::Token::MoreThan(0), right),
                    "<=" => {
                        cp::GeneralExpression::LessEqThan(left, ct::Token::LessEqThan(0), right)
                    }
                    ">=" => {
                        cp::GeneralExpression::MoreEqThan(left, ct::Token::MoreEqThan(0), right)
                    }
                    "==" => cp::GeneralExpression::Equals(left, ct::Token::Equals(0), right),
                    "!=" => cp::GeneralExpression::NotEquals(left, ct::Token::NotEquals(0), right),
                    "&" => cp::GeneralExpression::BitAnd(left, ct::Token::BitAnd(0), right),
                    "^" => cp::GeneralExpression::BitXor(left, ct::Token::BitXor(0), right),
                    "|" => cp::GeneralExpression::BitOr(left, ct::Token::BitOr(0), right),
                    "&&" => cp::GeneralExpression::And(left, ct::Token::And(0), right),
                    "||" => cp::GeneralExpression::Or(left, ct::Token::Or(0), right),

                    _ => unreachable!(),
                }
            }
            _ => cp::GeneralExpression::CastExpression(Box::new(self.cast_expression(k))),
        }
    }

    pub fn cast_expression(&mut self, k: pp::Expression) -> cp::CastExpression {
        match k {
            pp::Expression::Cast(expr, ty) => cp::CastExpression::OpenParen(
                ct::Token::OpenParen(0),
                Box::new(self.type_to_type_name(ty)),
                ct::Token::CloseParen(0),
                Box::new(self.general_expression(*expr)),
            ),
            _ => cp::CastExpression::UnaryExpression(self.unary_expression(k)),
        }
    }

    fn increment_or_decrement(&mut self, op: &str) -> cp::IncrementOrDecrement {
        match op {
            "++" => cp::IncrementOrDecrement::Increment(ct::Token::Increment(0)),
            "--" => cp::IncrementOrDecrement::Decrement(ct::Token::Decrement(0)),
            _ => unreachable!(),
        }
    }

    pub fn unary_expression(&mut self, k: pp::Expression) -> cp::UnaryExpression {
        match k {
            pp::Expression::Unary(op, list) => {
                let pp::ExprList::List(list) = list;
                assert_eq!(list.len(), 1);
                let arg = list[0].clone();
                match op.as_ref() {
                    "++" | "--" => cp::UnaryExpression::IncrementOrDecrement(
                        self.increment_or_decrement(op.as_ref()),
                        Box::new(self.unary_expression(arg)),
                    ),
                    op => {
                        let unary_op = match op {
                            "&" => cp::UnaryOperator::BitAnd(ct::Token::BitAnd(0)),
                            "*" => cp::UnaryOperator::Times(ct::Token::Times(0)),
                            "+" => cp::UnaryOperator::Plus(ct::Token::Plus(0)),
                            "-" => cp::UnaryOperator::Minus(ct::Token::Minus(0)),
                            "~" => cp::UnaryOperator::BitNot(ct::Token::BitNot(0)),
                            "!" => cp::UnaryOperator::Not(ct::Token::Not(0)),
                            _ => unreachable!(),
                        };
                        cp::UnaryExpression::UnaryOperator(
                            unary_op,
                            Box::new(self.cast_expression(arg)),
                        )
                    }
                }
            }
            pp::Expression::Sizeof(pp::Sizeof::Expression(e)) => {
                cp::UnaryExpression::SizeofExpr(Box::new(self.unary_expression(*e)))
            }
            pp::Expression::Sizeof(pp::Sizeof::Type(t)) => {
                cp::UnaryExpression::SizeofTy(Box::new(self.type_to_type_name(*t)))
            }
            _ => cp::UnaryExpression::PostfixExpression(Box::new(self.postfix_expression(k))),
        }
    }

    pub fn postfix_expression(&mut self, k: pp::Expression) -> cp::PostfixExpression {
        match k {
            pp::Expression::PostFix(expr, op) => {
                let inc_or_dec = match op.as_ref() {
                    "++" => cp::IncrementOrDecrement::Increment(ct::Token::Increment(0)),
                    "--" => cp::IncrementOrDecrement::Decrement(ct::Token::Decrement(0)),
                    _ => unreachable!(),
                };
                cp::PostfixExpression::Increment(
                    Box::new(self.postfix_expression(*expr)),
                    inc_or_dec,
                )
            }
            pp::Expression::ArrayAccess(array_expr, index_expr) => cp::PostfixExpression::Index(
                Box::new(self.postfix_expression(*array_expr)),
                ct::Token::OpenBracket(0),
                Box::new(self.expression(*index_expr)),
                ct::Token::CloseBracket(0),
            ),
            pp::Expression::Call(expr, pp::ArgList::Args(args)) => cp::PostfixExpression::Call(
                Box::new(self.postfix_expression(*expr)),
                ct::Token::OpenParen(0),
                cp::ArgumentExpressionList::List(
                    args.into_iter()
                        .map(|e| self.assignment_expression(e))
                        .collect(),
                ),
                ct::Token::CloseParen(0),
            ),
            pp::Expression::StructAccess(expr, ident) => cp::PostfixExpression::Member(
                Box::new(self.postfix_expression(*expr)),
                cp::MemberAccess::Dot(ct::Token::Dot(0)),
                ct::Token::Identifier(0, ident),
            ),
            _ => cp::PostfixExpression::PrimaryExpression(self.primary_expression(k)),
        }
    }

    pub fn primary_expression(&mut self, k: pp::Expression) -> cp::PrimaryExpression {
        match k {
            pp::Expression::PrimaryExpression(primary) => self.primary_to_primary(primary),
            _ => cp::PrimaryExpression::Expression(Box::new(self.expression(k))),
        }
    }

    pub fn primary_to_primary(&mut self, k: pp::PrimaryExpression) -> cp::PrimaryExpression {
        match k {
            pp::PrimaryExpression::Identifier(ident) => cp::PrimaryExpression::Identifier(ident),
            pp::PrimaryExpression::Literal(literal) => self.literal_to_primary(literal),
            pp::PrimaryExpression::BlockExpression(..) => unimplemented!(),
            pp::PrimaryExpression::Expression(expr) => {
                cp::PrimaryExpression::Expression(Box::new(self.expression(*expr)))
            }
            pp::PrimaryExpression::Lambda(..) => unreachable!(),
            pp::PrimaryExpression::StructInitialization(ident, fields) => {
                cp::PrimaryExpression::StructValue(
                    Box::new(
                        self.type_to_type_name(pp::TypeSignature::Struct(Some(ident), Vec::new())),
                    ),
                    fields
                        .iter()
                        .map(
                            |pp::StructInitializationField::StructInitializationField(
                                name,
                                expr,
                            )| (name, expr),
                        )
                        .map(|(name, field)| (name, self.assignment_expression(*field.clone())))
                        .map(|(name, expr)| {
                            cp::Initializer::Initializer(
                                Some(name.clone()),
                                Box::new(cp::AssignmentOrInitializerList::AssignmentExpression(
                                    expr,
                                )),
                            )
                        })
                        .collect(),
                )
            }
            pp::PrimaryExpression::VectorInitialization(ident, fields) => {
                cp::PrimaryExpression::StructValue(
                    Box::new(self.type_to_type_name(pp::TypeSignature::Plain(ident))),
                    fields
                        .into_iter()
                        .map(|field| self.assignment_expression(field))
                        .map(|expr| {
                            cp::Initializer::Initializer(
                                None,
                                Box::new(cp::AssignmentOrInitializerList::AssignmentExpression(
                                    expr,
                                )),
                            )
                        })
                        .collect(),
                )
            }
        }
    }

    pub fn literal_to_primary(&mut self, k: pp::Literal) -> cp::PrimaryExpression {
        match k {
            pp::Literal::Integer(pt::Token::Integer(_, i)) => {
                cp::PrimaryExpression::Number(From::from(i.to_string()))
            }
            pp::Literal::Float(pt::Token::Float(_, i)) => {
                cp::PrimaryExpression::Number(From::from(i.to_string()))
            }
            pp::Literal::StringLiteral(pt::Token::StringLiteral(_, i)) => {
                cp::PrimaryExpression::StringLiteral(From::from(i.to_string()))
            }
            _ => unreachable!(),
        }
    }
}

struct CToPurkka {
    types: HashMap<Rc<str>, TypeSignature>,
}

pub fn to_purkka(c_exps: Vec<cp::MacroExpansion>) -> Vec<pp::MacroExpansion> {
    let context = CToPurkka {types: HashMap::new()};
    c_exps.into_iter().map(|exp| context.macro_expansion(exp)).collect()
}

impl CToPurkka {
    fn macro_expansion(&self, exp: cp::MacroExpansion) -> pp::MacroExpansion {
        match exp {
            cp::MacroExpansion::Expression(e) => pp::MacroExpansion::Expression(self.expression(e)),
            cp::MacroExpansion::Statement(s) => pp::MacroExpansion::Statement(self.statement(s)),
            cp::MacroExpansion::Type(t) => pp::MacroExpansion::Type(self.type_name(t)),
        }
    }

    fn expression(&self, e: cp::Expression) -> pp::Expression {
        match e {
            cp::Expression::Expression(mut e) => {
                assert_eq!(e.len(), 1);
                self.assignment_expression(e.remove(0))
            }
        }
    }

    fn assignment_expression(&self, e: cp::AssignmentExpression) -> pp::Expression {
        match e {
            cp::AssignmentExpression::TernaryExpression(e) => self.ternary_expression(e),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    fn ternary_expression(&self, e: cp::TernaryExpression) -> pp::Expression {
        match e {
            cp::TernaryExpression::GeneralExpression(e) => self.general_expression(e),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    fn general_expression(&self, e: cp::GeneralExpression) -> pp::Expression {
        match e {
            cp::GeneralExpression::CastExpression(e) => self.cast_expression(*e),
            cp::GeneralExpression::Plus(left, _, right) =>
                pp::Expression::Op(From::from("+"),
                pp::ExprList::List(vec![self.general_expression(*left), self.general_expression(*right)])),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    fn cast_expression(&self, e: cp::CastExpression) -> pp::Expression {
        match e {
            cp::CastExpression::UnaryExpression(e) => self.unary_expression(e),
            cp::CastExpression::OpenParen(_, ty, _, e) => pp::Expression::Cast(
                Box::new(self.general_expression(*e)),
                self.type_name(*ty)
            ), 
            other => panic!("Not implemented: {:?}", other),
        }
    }

    fn unary_expression(&self, e: cp::UnaryExpression) -> pp::Expression {
        match e {
            cp::UnaryExpression::PostfixExpression(e) => self.postfix_expression(*e),
            cp::UnaryExpression::SizeofTy(ty) => pp::Expression::Sizeof(pp::Sizeof::Type(Box::new(self.type_name(*ty)))),
            cp::UnaryExpression::UnaryOperator(cp::UnaryOperator::BitAnd(_), expr) =>
                pp::Expression::Unary(From::from("&"),
                pp::ExprList::List(vec![self.cast_expression(*expr)])),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    fn postfix_expression(&self, e: cp::PostfixExpression) -> pp::Expression {
        match e {
            cp::PostfixExpression::PrimaryExpression(e) => pp::Expression::PrimaryExpression(self.primary_expression(e)),
            cp::PostfixExpression::Call(e, _, cp::ArgumentExpressionList::List(arglist), _) => pp::Expression::Call(
                Box::new(self.postfix_expression(*e)),
                pp::ArgList::Args(arglist.into_iter().map(|e| self.assignment_expression(e)).collect())
            ),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    fn primary_expression(&self, e: cp::PrimaryExpression) -> pp::PrimaryExpression {
        match e {
            cp::PrimaryExpression::Expression(e) => pp::PrimaryExpression::Expression(Box::new(self.expression(*e))),
            cp::PrimaryExpression::Number(e) => pp::PrimaryExpression::Literal(pp::Literal::Integer(pt::Token::Integer(0, e.parse().unwrap()))),
            cp::PrimaryExpression::Identifier(ident) => pp::PrimaryExpression::Identifier(ident),
            other => panic!("Not implemented: {:?}", other),
        }
    }

    fn statement(&self, _e: cp::Statement) -> pp::Statement {
        unimplemented!()
    }

    fn type_name(&self, cp::TypeName::TypeName(spec, decl): cp::TypeName) -> pp::TypeSignature {
        let ty = self.decl_spec_to_type(&*spec, Vec::new());
        self.abstract_declarator_to_type(&*decl, ty)
    }

    fn function_definition_to_type(
        &self,
        decl: &cp::FunctionDefinition,
    ) -> (Declarations, TypeSignature) {
        let cp::FunctionDefinition::FunctionDefinition(spec, decl, _) = decl;
        let spec_ty = self.decl_spec_to_type(spec.as_ref().unwrap(), Vec::new());
        (
            vec![self.declarator_to_type(decl, spec_ty.clone())],
            spec_ty,
        )
    }

    fn declaration_to_type(&self, decl: &cp::Declaration) -> (Declarations, bool, TypeSignature) {
        match decl {
            cp::Declaration::Declaration(spec, init_decls, attrs) => {
                let spec_ty =
                    self.decl_spec_to_type(&**spec, attrs.clone().unwrap_or_else(Vec::new));
                let decl_tys = init_decls
                    .iter()
                    .map(|init_decl| match init_decl {
                        cp::InitDeclarator::Declarator(decl)
                        | cp::InitDeclarator::Asm(decl, _)
                        | cp::InitDeclarator::Assign(decl, _, _) => {
                            self.declarator_to_type(&**decl, spec_ty.clone())
                        }
                    })
                    .collect();
                (decl_tys, has_typedef(&**spec), spec_ty)
            }
            cp::Declaration::Pragma(..) => unreachable!(),
        }
    }

    fn decl_spec_to_type(
        &self,
        spec: &cp::DeclarationSpecifiers,
        attrs: Vec<cp::Attribute>,
    ) -> TypeSignature {
        let cp::DeclarationSpecifiers::DeclarationSpecifiers(_, ty) = spec;
        ty.as_ref()
            .map(|t| self.type_specifier_to_type(t.clone(), attrs))
            .unwrap()
    }

    fn declarator_to_type(&self, decl: &cp::Declarator, ty: TypeSignature) -> (Rc<str>, TypeSignature) {
        let cp::Declarator::Declarator(ptr, decl) = decl;
        let ty = if let Some(p) = ptr {
            self.ptr_to_ty(p, Box::new(ty))
        } else {
            ty
        };
        self.direct_decl_to_type(decl, ty)
    }

    fn abstract_declarator_to_type(
        &self,
        decl: &cp::AbstractDeclarator,
        ty: TypeSignature,
    ) -> TypeSignature {
        let cp::AbstractDeclarator::AbstractDeclarator(ptr, decl) = decl;
        let ty = if let Some(p) = ptr {
            self.ptr_to_ty(p, Box::new(ty))
        } else {
            ty
        };
        self.direct_abstract_decl_to_type(&**decl, ty)
    }

    fn direct_decl_to_type(
        &self,
        decl: &cp::DirectDeclarator,
        ty: TypeSignature,
    ) -> (Rc<str>, TypeSignature) {
        match decl {
            cp::DirectDeclarator::Identifier(ident) => (ident.clone(), ty),
            cp::DirectDeclarator::AsmStatement(_) => (From::from("_"), ty),
            cp::DirectDeclarator::Parens(decl) => self.declarator_to_type(&**decl, ty),
            cp::DirectDeclarator::Array(decl, None) => {
                let (name, ty) = self.direct_decl_to_type(&**decl, ty);
                (name, TypeSignature::Array(Box::new(ty), None))
            }
            cp::DirectDeclarator::Array(decl, Some(_e)) => {
                let (name, ty) = self.direct_decl_to_type(&**decl, ty);
                (name, TypeSignature::Array(Box::new(ty), Some(0)))
            }
            cp::DirectDeclarator::Function(decl, params) => {
                let (name, ty) = self.direct_decl_to_type(&**decl, ty);
                (
                    name,
                    TypeSignature::Function(
                        params.iter().map(|f| self.function_param(f)).collect(),
                        Box::new(ty),
                    ),
                )
            }
        }
    }

    fn direct_abstract_decl_to_type(
        &self,
        decl: &cp::DirectAbstractDeclarator,
        ty: TypeSignature,
    ) -> TypeSignature {
        match decl {
            cp::DirectAbstractDeclarator::Epsilon() => ty,
            cp::DirectAbstractDeclarator::Parens(decl) => self.abstract_declarator_to_type(&**decl, ty),
            cp::DirectAbstractDeclarator::Array(decl, None) => {
                let ty = self.direct_abstract_decl_to_type(&**decl, ty);
                TypeSignature::Array(Box::new(ty), None)
            }
            cp::DirectAbstractDeclarator::Array(decl, Some(_e)) => {
                let ty = self.direct_abstract_decl_to_type(&**decl, ty);
                TypeSignature::Array(Box::new(ty), Some(0))
            }
            cp::DirectAbstractDeclarator::Function(decl, params) => {
                let ty = self.direct_abstract_decl_to_type(&**decl, ty);
                TypeSignature::Function(
                    params.iter().map(|f| self.function_param(f)).collect(),
                    Box::new(ty),
                )
            }
        }
    }

    fn ptr_to_ty(&self, ptr: &cp::Pointer, ty: Box<TypeSignature>) -> TypeSignature {
        let cp::Pointer::Ptr(_, p) = ptr;
        if let Some(p) = p {
            self.ptr_to_ty(p, Box::new(TypeSignature::Pointer { ty, nullable: true }))
        } else {
            TypeSignature::Pointer { ty, nullable: true }
        }
    }

    fn type_specifier_to_type(
        &self,
        mut ty: cp::TypeSpecifier,
        attrs: Vec<cp::Attribute>,
    ) -> TypeSignature {
        use cp::CType::Primitive as CP;
        use cp::PrimitiveType::*;
        use pp::TypeSignature::Primitive as P;

        let mut complex = false;

        if let cp::CType::Complex(sign, t) = ty {
            ty = cp::CType::Primitive(sign, t);
            complex = true;
        }

        let ty = match ty {
            cp::CType::Void => P(Primitive::Void),
            CP(None, Char) | CP(Some(true), Char) => P(Primitive::Int(8)),
            CP(Some(false), Char) => P(Primitive::UInt(8)),

            CP(None, Short) | CP(Some(true), Short) => P(Primitive::Int(16)),
            CP(Some(false), Short) => P(Primitive::UInt(16)),

            CP(None, Int) | CP(Some(true), Int) => P(Primitive::Int(32)),
            CP(Some(false), Int) => P(Primitive::UInt(32)),

            CP(None, Long)
            | CP(Some(true), Long)
            | CP(None, LongLong)
            | CP(Some(true), LongLong) => P(Primitive::Int(64)),
            CP(Some(false), Long) | CP(Some(false), LongLong) => P(Primitive::UInt(64)),

            CP(None, Float) | CP(Some(true), Float) => P(Primitive::Float),
            CP(Some(false), Float) => P(Primitive::Float),

            CP(None, Double) | CP(Some(true), Double) => P(Primitive::Double),
            CP(Some(false), Double) => P(Primitive::Double),

            CP(None, LongDouble) | CP(Some(true), LongDouble) => P(Primitive::Double),
            CP(Some(false), LongDouble) => P(Primitive::Double),

            cp::CType::Compound(compound) => self.compound_type_to_type(&compound),
            cp::CType::Custom(ty) => TypeSignature::Plain(ty.clone()),
            cp::CType::Complex(..) => unreachable!(),
        };

        if let P(prim) = ty {
            if complex {
                return TypeSignature::Complex(prim);
            }
        }

        if let P(prim) = ty {
            if !attrs.is_empty() {
                return TypeSignature::Vector(prim);
            }
        }

        ty
    }

    fn compound_type_to_type(&self, ty: &cp::CompoundType) -> TypeSignature {
        match ty {
            cp::CompoundType::Struct(name, Some(fields)) => TypeSignature::Struct(
                Some(name.clone()),
                fields
                    .iter()
                    .flat_map(|f| self.struct_field(f).into_iter())
                    .collect(),
            ),
            cp::CompoundType::Struct(name, None) => self
                .types
                .get(name)
                .cloned()
                .unwrap_or_else(|| TypeSignature::Plain(name.clone())),
            cp::CompoundType::AnonymousStruct(fields) => TypeSignature::Struct(
                None,
                fields
                    .iter()
                    .flat_map(|f| self.struct_field(f).into_iter())
                    .collect(),
            ),
            cp::CompoundType::Union(name, Some(fields)) => TypeSignature::Union(
                Some(name.clone()),
                fields
                    .iter()
                    .flat_map(|f| self.struct_field(f).into_iter())
                    .collect(),
            ),
            cp::CompoundType::Union(name, None) => self.types[name].clone(),
            cp::CompoundType::AnonymousUnion(fields) => TypeSignature::Union(
                None,
                fields
                    .iter()
                    .flat_map(|f| self.struct_field(f).into_iter())
                    .collect(),
            ),
            cp::CompoundType::Enum(name, Some(fields)) => TypeSignature::Enum(
                Some(name.clone()),
                fields.iter().map(|f| self.enum_field(f)).collect(),
            ),
            cp::CompoundType::Enum(name, None) => self.types[name].clone(),
            cp::CompoundType::AnonymousEnum(fields) => {
                TypeSignature::Enum(None, fields.iter().map(|f| self.enum_field(f)).collect())
            }
        }
    }

    fn struct_field(&self, f: &cp::StructField) -> Vec<purkkasyntax::StructField> {
        let spec_ty = self.decl_spec_to_type(&*f.0, Vec::new());
        f.1.iter()
            .map(|(decl, maybe_bitfield)| {
                let bitfield = maybe_bitfield
                    .as_ref()
                    .map(|b| usize::try_from(b.value().unwrap()).unwrap());

                let (name, ty) = match decl {
                    cp::EitherDeclarator::Anonymous(_) => unimplemented!(),
                    cp::EitherDeclarator::Declarator(decl) => {
                        self.declarator_to_type(decl, spec_ty.clone())
                    }
                };
                purkkasyntax::StructField::Field {
                    name,
                    ty: Box::new(ty),
                    bitfield,
                }
            })
            .collect()
    }

    fn enum_field(&self, f: &cp::EnumField) -> purkkasyntax::EnumField {
        purkkasyntax::EnumField::Field {
            name: f.0.clone(),
            value: f.2,
            ty: None,
        }
    }

    fn function_param(&self, f: &cp::FunctionParam) -> purkkasyntax::Param {
        match f {
            cp::FunctionParam::Identifier(_name) => unimplemented!(),
            cp::FunctionParam::Parameter(param) => match param {
                cp::ParameterDeclaration::Declarator(spec, decl) => {
                    let spec_ty = self.decl_spec_to_type(spec, Vec::new());
                    let (name, ty) = self.declarator_to_type(decl, spec_ty);
                    purkkasyntax::Param::Param(name, Box::new(ty))
                }
                cp::ParameterDeclaration::AbstractDeclarator(spec, decl) => {
                    let spec_ty = self.decl_spec_to_type(spec, Vec::new());
                    let ty = self.abstract_declarator_to_type(decl, spec_ty);
                    purkkasyntax::Param::Param(From::from("_"), Box::new(ty))
                }
                cp::ParameterDeclaration::DeclarationSpecifiers(spec) => {
                    let spec_ty = self.decl_spec_to_type(spec, Vec::new());
                    purkkasyntax::Param::Param(From::from("_"), Box::new(spec_ty))
                }
            },
            cp::FunctionParam::Varargs => purkkasyntax::Param::Variadic,
        }
    }
}

fn has_typedef(spec: &cp::DeclarationSpecifiers) -> bool {
    if let cp::DeclarationSpecifiers::DeclarationSpecifiers(Some(spec), _) = spec {
        spec.0.typedef
    } else {
        false
    }
}

trait ReplaceEmpty<T> {
    fn replace_if_empty(&mut self, t: T, e: &str);
}

impl<T> ReplaceEmpty<T> for Option<T>
where
    T: std::fmt::Debug,
{
    fn replace_if_empty(&mut self, t: T, e: &str) {
        if self.is_some() {
            panic!(
                "{} (tried to replace {:?} with {:?})",
                e,
                self.as_ref().unwrap(),
                t
            );
        }

        self.replace(t);
    }
}

