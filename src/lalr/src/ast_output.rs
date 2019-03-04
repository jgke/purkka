use rustc_target::spec::abi::Abi;
use smallvec::SmallVec;
use syntax::ast;
use syntax::ext::base::{ExtCtxt, MacEager, MacResult};
use syntax::ext::build::AstBuilder;
use syntax_pos::{FileName, Span};
use syntax::parse;
use syntax::ptr::P;
use syntax::source_map::{dummy_spanned, respan, Spanned};
use syntax_pos::symbol;

use std::collections::HashSet;
use std::iter;

use types::{Action, LRTable, Rule, RuleData, RuleTranslationMap};

struct AstBuilderCx<'a, 'c> {
    cx: &'a ExtCtxt<'c>,
    span: Span,
    tm: &'a RuleTranslationMap,
    lalr_table: &'a LRTable,
    terminals: &'a HashSet<(String, String)>,
}

impl<'a, 'c> AstBuilderCx<'a, 'c> {
    fn ty_ident(&self, s: &str) -> P<ast::Ty> {
        self.cx.ty_ident(self.span, self.cx.ident_of(s))
    }

    fn item_derive(&self, span: Span, ident: ast::Ident, kind: ast::ItemKind) -> P<ast::Item> {
        let derive = self.cx.attribute(
            span,
            self.cx.meta_list(
                span,
                symbol::Symbol::intern("derive"),
                vec![
                    self.cx
                        .meta_list_item_word(span, symbol::Symbol::intern("Clone")),
                    self.cx
                        .meta_list_item_word(span, symbol::Symbol::intern("Debug")),
                    self.cx
                        .meta_list_item_word(span, symbol::Symbol::intern("PartialEq")),
                ],
            ),
        );
        let mut item = self.cx.item(span, ident, vec![derive], kind);
        item.vis = respan(span.shrink_to_lo(), ast::VisibilityKind::Public);
        item
    }
    fn item_enum_derive(
        &self,
        span: Span,
        ident: ast::Ident,
        enumdef: ast::EnumDef,
    ) -> P<ast::Item> {
        self.item_derive(
            span,
            ident,
            ast::ItemKind::Enum(enumdef, ast::Generics::default()),
        )
    }
    fn item_struct_derive(
        &self,
        span: Span,
        ident: ast::Ident,
        structdef: ast::VariantData,
    ) -> P<ast::Item> {
        self.item_derive(
            span,
            ident,
            ast::ItemKind::Struct(structdef, ast::Generics::default()),
        )
    }

    fn boxed(&self, ty: P<ast::Ty>) -> P<ast::Ty> {
        self.cx.ty_path(self.cx.path_all(
            self.span,
            true,
            self.cx.std_path(&["boxed", "Box"]),
            vec![ast::GenericArg::Type(ty)],
            Vec::new(),
        ))
    }

    fn sliced(&self, ty: P<ast::Ty>, count: usize) -> P<ast::Ty> {
        self.cx.ty(
            self.span,
            ast::TyKind::Array(
                ty,
                ast::AnonConst {
                    id: ast::DUMMY_NODE_ID,
                    value: self.cx.expr_usize(self.span, count),
                },
            ),
        )
    }

    fn get_plain_variant(&self, span: Span, ty: &str) -> ast::Variant {
        self.cx
            .variant(span, self.cx.ident_of(ty), vec![self.ty_ident(ty)])
    }

    fn get_enum_item(&self, rule: &Rule) -> Vec<P<ast::Item>> {
        let enum_name = &rule.identifier;
        let mut enum_items: Vec<P<ast::Item>> = rule
            .data
            .clone()
            .into_iter()
            .map(|(variant_identifier, components)| {
                let mut total_span = components.get(0).unwrap().span;
                let vals = components
                    .into_iter()
                    .map(|item| {
                        total_span = total_span.to(item.span);
                        //let (n, x, s) = item;
                        let ident_arr = item.full_path.rsplitn(2, "::");
                        // .unwrap() here is always safe
                        let ident_ty = self.ty_ident(ident_arr.last().unwrap());
                        let ty;
                        if item.indirect || &item.identifier == enum_name {
                            ty = self.boxed(ident_ty);
                        } else {
                            ty = ident_ty;
                        }
                        ast::StructField {
                            span: item.span,
                            ty,
                            ident: None,
                            vis: respan(item.span.shrink_to_lo(), ast::VisibilityKind::Inherited),
                            attrs: Vec::new(),
                            id: ast::DUMMY_NODE_ID,
                        }
                    })
                    .collect();
                let struct_name = format!("{}_{}", rule.identifier, variant_identifier);
                let item = self.item_struct_derive(
                    total_span,
                    self.cx.ident_of(&struct_name),
                    ast::VariantData::Tuple(vals, ast::DUMMY_NODE_ID),
                );
                item
            })
            .collect();
        let enumdef = ast::EnumDef {
            variants: rule
                .data
                .clone()
                .into_iter()
                .map(|(variant_identifier, components)| {
                    let var_id = &format!("{}_{}", rule.identifier, variant_identifier);
                    let total_span = components.get(0).unwrap().span;
                    self.cx.variant(
                        total_span,
                        self.cx.ident_of(&variant_identifier),
                        vec![self.ty_ident(var_id)],
                    )
                })
                .collect(),
        };

        let ident = self.cx.ident_of(enum_name);
        enum_items.push(self.item_enum_derive(rule.span, ident, enumdef));
        enum_items
    }

    fn terminal_pattern(&self, symbol: &String) -> P<ast::Pat> {
        self.cx.pat(
            self.span,
            ast::PatKind::TupleStruct(
                self.cx.path(
                    self.span,
                    symbol.split("::").map(|x| self.cx.ident_of(x)).collect(),
                ),
                vec![],
                Some(0),
            ),
        )
    }

    fn get_translation_fn_body(&self) -> P<ast::Block> {
        let dollar_id = self.usize_expr(self.tm.indices["$"]);
        let token_unwrap = self.cx.expr_method_call(
            self.span,
            self.cx.expr_ident(self.span, self.cx.ident_of("token")),
            self.cx.ident_of("unwrap"),
            vec![],
        );

        self.cx.block(
            self.span,
            vec![
                self.cx.stmt_expr(
                    //if token.is_none() { return token id for $ }
                    self.cx.expr_if(
                        self.span,
                        self.cx.expr_method_call(
                            self.span,
                            self.cx.expr_ident(self.span, self.cx.ident_of("token")),
                            self.cx.ident_of("is_none"),
                            vec![],
                        ),
                        self.cx.expr(self.span, ast::ExprKind::Ret(Some(dollar_id))),
                        None,
                    ),
                ),
                self.cx.stmt_expr(
                    self.cx.expr_match(
                        self.span,
                        token_unwrap,
                        self.terminals
                            .iter()
                            .map(|(_, terminal)| {
                                self.cx.arm(
                                    self.span,
                                    vec![self.terminal_pattern(&terminal)],
                                    self.cx.expr_usize(self.span, self.tm.indices[terminal]),
                                )
                            })
                            .chain(iter::once(self.wild_arm_unreachable()))
                            .collect(),
                    ),
                ),
            ],
        )
    }

    pub fn get_translation_fn(&self) -> P<ast::Item> {
        let ty_usize = self.ty_ident("usize");
        let terminal_base = &self
            .terminals
            .iter()
            .next()
            .unwrap()
            .1
            .rsplitn(2, "::")
            .last()
            .unwrap();
        let ty_return = self.cx.ty_rptr(
            self.span,
            self.ty_ident(terminal_base),
            None,
            ast::Mutability::Immutable,
        );
        let body = self.get_translation_fn_body();
        let span = self.span;
        let name = self.cx.ident_of("_convert_token_to_index");
        let inputs = vec![self.cx.arg(
            self.span,
            self.cx.ident_of("token"),
            self.cx.ty_option(ty_return),
            )];
        let output = ty_usize;

        // #[allow(unreachable_patterns)]
        let unreachable_patterns = self.cx.attribute(
            self.span,
            self.cx.meta_list(
                self.span,
                symbol::Symbol::intern("allow"),
                vec![
                    self.cx
                        .meta_list_item_word(self.span, symbol::Symbol::intern("unreachable_patterns")),
                ],
            ),
        );

        self.cx.item(span,
                  name,
                  vec![unreachable_patterns],
                  ast::ItemKind::Fn(self.cx.fn_decl(inputs, ast::FunctionRetTy::Ty(output)),
                              ast::FnHeader {
                                  unsafety: ast::Unsafety::Normal,
                                  asyncness: ast::IsAsync::NotAsync,
                                  constness: dummy_spanned(ast::Constness::NotConst),
                                  abi: Abi::Rust,
                              },
                              ast::Generics::default(),
                              body))
    }

    pub fn get_translation_enum(&self) -> P<ast::Item> {
        let enumdef = ast::EnumDef {
            variants: self
                .tm
                .rules
                .iter()
                .flat_map(|(name, rule)| {
                    let tmp: Vec<_> = rule
                        .data
                        .iter()
                        .map(|(variant, components)| {
                            let var_id = self.cx.ident_of(&format!("{}_{}", name, variant));
                            let span = components.get(0).unwrap().span;
                            self.cx
                                .variant(span, var_id, vec![self.cx.ty_ident(self.span, var_id)])
                        })
                        .collect();
                    tmp
                })
                .collect(),
        };

        let ident = self.cx.ident_of("_lalr_symbols");
        self.item_enum_derive(self.span, ident, enumdef)
    }

    pub fn enum_variant(&self, name: &str, args: Vec<&str>) -> Spanned<ast::Variant_> {
        self.cx.variant(
            self.span,
            self.cx.ident_of(name),
            args.iter().map(|x| self.ty_ident(x)).collect(),
        )
    }

    pub fn get_action_enum(&self) -> P<ast::Item> {
        let enumdef = ast::EnumDef {
            variants: vec![
                self.enum_variant("Error", vec![]),
                self.enum_variant("Shift", vec!["usize"]),
                self.enum_variant("Reduce", vec!["usize", "usize", "usize"]),
                self.enum_variant("Accept", vec![]),
                self.enum_variant("Goto", vec!["usize"]),
            ],
        };

        let ident = self.cx.ident_of("_Act");
        self.item_enum_derive(self.span, ident, enumdef)
    }

    fn action_expr(&self, args: (&str, Option<Vec<P<ast::Expr>>>)) -> P<ast::Expr> {
        let act = self.cx.path(
            self.span,
            vec![self.cx.ident_of("_Act"), self.cx.ident_of(args.0)],
        );
        match args.1 {
            Some(expr) => self.cx.expr_call(self.span, self.cx.expr_path(act), expr),
            None => self.cx.expr_path(act),
        }
    }

    fn usize_expr(&self, i: usize) -> P<ast::Expr> {
        self.cx.expr_lit(
            self.span,
            ast::LitKind::Int(i as u128, ast::LitIntType::Unsuffixed),
        )
    }

    fn lalr_action_call(&self, action: &Action) -> P<ast::Expr> {
        self.action_expr(match action {
            Action::Error => ("Error", None),
            Action::Shift(pat) => ("Shift", Some(vec![self.usize_expr(*pat)])),
            Action::Reduce(ident, subrule, count) => (
                "Reduce",
                Some(vec![
                    self.usize_expr(self.tm.indices[ident]),
                    self.usize_expr(*subrule),
                    self.usize_expr(*count),
                ]),
            ),
            Action::Accept => ("Accept", None),
            Action::Goto(pat) => ("Goto", Some(vec![self.usize_expr(*pat)])),
        })
    }

    fn lalr_table_row(&self, actions: &Vec<Action>) -> P<ast::Expr> {
        self.cx.expr_vec(
            self.span,
            actions
                .iter()
                .map(|act| self.lalr_action_call(act))
                .collect(),
        )
    }

    fn get_full_lalr_table(&self) -> Vec<Vec<Action>> {
        let mut rows: Vec<(&String, usize)> = self
            .tm
            .indices
            .iter()
            .map(|(name, index)| (name, *index))
            .collect();
        rows.sort_unstable_by(|(_, index1), (_, index2)| index1.cmp(index2));
        self.lalr_table
            .actions
            .iter()
            .enumerate()
            .map(|(index, _)| {
                rows.iter()
                    .map(|(lookahead, _)| {
                        self.lalr_table
                            .clone()
                            .actions
                            .get(index)
                            .unwrap()
                            .get(*lookahead)
                            .unwrap_or(&Action::Error)
                    })
                    .map(|x| x.clone())
                    .collect()
            })
            .collect()
    }

    pub fn get_lalr_table(&self) -> P<ast::Item> {
        let act_ty = self.ty_ident("_Act");
        let count = self.tm.indices.len();
        let ty_return = self.sliced(self.sliced(act_ty, count), self.lalr_table.actions.len());
        let content = self.get_full_lalr_table();
        self.cx.item_static(
            self.span,
            self.cx.ident_of("_STATE_TABLE"),
            ty_return,
            ast::Mutability::Immutable,
            self.cx.expr_vec(
                self.span,
                content.iter().map(|x| self.lalr_table_row(x)).collect(),
            ),
        )
    }
    pub fn get_starter_var(&self) -> P<ast::Item> {
        let ty_var = self.ty_ident("usize");
        self.cx.item_static(
            self.span,
            self.cx.ident_of("_STARTER_VALUE"),
            ty_var,
            ast::Mutability::Immutable,
            self.usize_expr(0),
        )
    }

    fn get_wrapper_fn_body(&self, terminals: &HashSet<(String, String)>) -> P<ast::Block> {
        self.cx.block(
            self.span,
            vec![self.cx.stmt_expr(
                // match index {
                self.cx.expr_match(
                    self.span,
                    self.cx.expr_ident(self.span, self.cx.ident_of("index")),
                    terminals
                        .iter()
                        .map(|(name, p)| {
                            // 3 => reduction_match_body
                            self.cx.arm(
                                self.span,
                                vec![self
                                    .cx
                                    .pat_lit(self.span, self.usize_expr(self.tm.indices[p]))],
                                self.box_expr(self.cx.expr_call(
                                    self.span,
                                    self.cx.expr_path(self.cx.path(
                                        self.span,
                                        vec![self.cx.ident_of("_Data"), self.cx.ident_of(name)],
                                    )),
                                    vec![self.box_or_star(
                                        false,
                                        self.cx.expr_ident(self.span, self.cx.ident_of("token")),
                                    )],
                                )),
                            )
                        })
                        .chain(iter::once(self.wild_arm_unreachable()))
                        .collect(),
                ),
                // }
            )],
        )
    }
    pub fn get_wrapper_fn(&self, terminals: &HashSet<(String, String)>) -> P<ast::Item> {
        let ty_return = self.boxed(self.ty_ident("_Data"));
        let body = self.get_wrapper_fn_body(terminals);
        self.cx.item_fn(
            self.span,
            self.cx.ident_of("_token_to_data"),
            vec![
                self.cx
                    .arg(self.span, self.cx.ident_of("index"), self.ty_ident("usize")),
                self.cx.arg(
                    self.span,
                    self.cx.ident_of("token"),
                    self.cx.ty_rptr(
                        self.span,
                        self.ty_ident("Token"),
                        None,
                        ast::Mutability::Immutable,
                    ),
                ),
            ],
            ty_return,
            body,
        )
    }

    fn reduction_match_pat(&self, identifier: &str, index: usize) -> P<ast::Pat> {
        self.cx.pat_tuple_struct(
            self.span,
            self.cx.path(
                self.span,
                vec![self.cx.ident_of("_Data"), self.cx.ident_of(identifier)],
            ),
            vec![self
                .cx
                .pat_ident(self.span, self.cx.ident_of(&format!("pat_{}", index)))],
        )
    }

    fn reduction_match_data(
        &self,
        subindex: usize,
        data: &Vec<RuleData>,
    ) -> P<ast::Pat> {
        let slice_pat: P<ast::Pat> = self.cx.pat(
            self.span,
            ast::PatKind::Slice(
                data.iter()
                    .enumerate()
                    .map(|(i, ruledata)| {
                        self.cx.pat(
                            self.span,
                            ast::PatKind::Box(self.reduction_match_pat(&ruledata.identifier, i)),
                        )
                    })
                    .collect(),
                None,
                vec![],
            ),
        );
        self.cx.pat_tuple(
            self.span,
            vec![
                self.cx.pat_lit(self.span, self.usize_expr(subindex)),
                slice_pat,
            ],
        )
        /*
         * let a = match (subindex, data) {
         *    1, [_Data::A(a), _Data::B(b)] => _Data::C(a, b),
         *    ...,
         *    _ => panic!("Unreachable")
         * }
         */
    }
    fn wild_arm_fail(&self, func: &str) -> ast::Arm {
        let session = self.cx.parse_sess();
        let filename = FileName::Custom("lalr_driver_fn_coerce_panic".to_string());
        let unreachable_expr = format!("{{ panic!(\"{{}}, {{}} {{}}\", index, subindex, \"{}\") }}", func);
        let expr = parse::new_parser_from_source_str(session, filename, unreachable_expr).parse_expr().unwrap();
        self.cx.arm( self.span, vec![self.cx.pat_wild(self.span)], expr)
    }
    fn wild_arm_unreachable(&self) -> ast::Arm {
        let session = self.cx.parse_sess();
        let filename = FileName::Custom("lalr_driver_fn_unreachable".to_string());
        let unreachable_expr = "{ unreachable!() }".to_string();
        let expr = parse::new_parser_from_source_str(session, filename, unreachable_expr).parse_expr().unwrap();
        self.cx.arm( self.span, vec![self.cx.pat_wild(self.span)], expr)
    }
    fn box_expr(&self, expr: P<ast::Expr>) -> P<ast::Expr> {
        self.cx.expr_call_global(
            self.span,
            self.cx.std_path(&["boxed", "Box", "new"]),
            vec![expr],
        )
    }
    fn box_or_star(&self, boxed: bool, expr: P<ast::Expr>) -> P<ast::Expr> {
        let starred = self
            .cx
            .expr_method_call(self.span, expr, self.cx.ident_of("clone"), vec![]);
        if boxed {
            self.box_expr(starred)
        } else {
            starred
        }
    }
    fn reduction_match_body(
        &self,
        name: &str,
        data: &Vec<(String, Vec<RuleData>)>,
    ) -> P<ast::Expr> {
        dbg!(&name);
        dbg!(&data.len());
        let bodies = data
            .iter()
            .enumerate()
            .map(|(subindex, (variant_name, ruledata))| {
                if ruledata.len() == 1 && ruledata[0].identifier == "Epsilon" {
                    self.cx.arm(
                        self.span,
                        vec![self.cx.pat_wild(self.span)],
                        self.box_expr(self.cx.expr_call(
                            self.span,
                            self.cx.expr_path(self.cx.path(
                                self.span,
                                vec![self.cx.ident_of("_Data"), self.cx.ident_of("Epsilon")],
                            )),
                            vec![
                                self.cx.expr_path(self.cx.path(
                                    self.span,
                                    vec![self.cx.ident_of("Epsilon"), self.cx.ident_of("Epsilon")],
                                )),
                            ],
                        ))
                    )
                } else {
                    self.cx.arm(
                        self.span,
                        vec![self.reduction_match_data(subindex, ruledata)],
                        self.box_expr(self.cx.expr_call(
                            self.span,
                            self.cx.expr_path(self.cx.path(
                                self.span,
                                vec![self.cx.ident_of("_Data"), self.cx.ident_of(name)],
                            )),
                            vec![self.cx.expr_call(
                                self.span,
                                self.cx.expr_path(self.cx.path(
                                    self.span,
                                    vec![self.cx.ident_of(name), self.cx.ident_of(variant_name)],
                                )),
                                vec![
                            self.cx.expr_call(
                                self.span,
                                self.cx.expr_path(self.cx.path_ident(
                                        self.span,
                                        self.cx.ident_of(&format!("{}_{}", name, variant_name)))),
                                ruledata.iter().enumerate()
                                .map(|(i, data)|
                                     self.box_or_star(
                                         data.indirect,
                                         self.cx.expr_ident(self.span,
                                                            self.cx.ident_of(
                                                                &format!("pat_{}", i)))))
                                .collect())],
                            )],
                        )),
                    )
                }
            });
        let eps_chain = if name == "Epsilon" || name == "$" {
            iter::once(
                self.cx.arm(
                    self.span,
                    vec![self.cx.pat_wild(self.span)],
                    self.box_expr(self.cx.expr_call(
                        self.span,
                        self.cx.expr_path(self.cx.path(
                            self.span,
                            vec![self.cx.ident_of("_Data"), self.cx.ident_of("Epsilon")],
                        )),
                        vec![
                            self.cx.expr_path(self.cx.path(
                                self.span,
                                vec![self.cx.ident_of("Epsilon"), self.cx.ident_of("Epsilon")],
                            )),
                        ],
                    ))
                ),
            )
        } else {
            iter::once( self.wild_arm_fail("coerce_panic"))
        };
        self.cx.expr_match(
            self.span,
            self.cx.expr_tuple(
                self.span,
                vec![
                    self.cx.expr_ident(self.span, self.cx.ident_of("subindex")),
                    self.cx.expr_ident(self.span, self.cx.ident_of("data")),
                ],
            ),
            bodies
                .chain(eps_chain)
                .collect(),
        )
    }
    fn get_reduction_fn_body(&self, rules: &Vec<Rule>) -> P<ast::Block> {
        self.cx.block(
            self.span,
            vec![self.cx.stmt_expr(
                // match index {
                self.cx.expr_match(
                    self.span,
                    self.cx.expr_ident(self.span, self.cx.ident_of("index")),
                    rules
                        .iter()
                        .map(|rule| {
                            // 3 => reduction_match_body
                            self.cx.arm(
                                self.span,
                                vec![self.cx.pat_lit(
                                    self.span,
                                    self.usize_expr(self.tm.indices[&rule.identifier]),
                                )],
                                self.reduction_match_body(&rule.identifier, &rule.data),
                            )
                        })
                        .chain(iter::once(self.wild_arm_unreachable()))
                        .collect(),
                ),
                // }
            )],
        )
    }
    pub fn get_reduction_fn(&self, rules: &Vec<Rule>) -> P<ast::Item> {
        let ty_return = self.boxed(self.ty_ident("_Data"));
        let body = self.get_reduction_fn_body(rules);
        self.cx.item_fn(
            self.span,
            self.cx.ident_of("_reduce_to_ast"),
            vec![
                self.cx
                    .arg(self.span, self.cx.ident_of("index"), self.ty_ident("usize")),
                self.cx.arg(
                    self.span,
                    self.cx.ident_of("subindex"),
                    self.ty_ident("usize"),
                ),
                self.cx.arg(
                    self.span,
                    self.cx.ident_of("data"),
                    self.cx.ty_rptr(
                        self.span,
                        self.cx.ty(self.span, ast::TyKind::Slice(ty_return.clone())),
                        None,
                        ast::Mutability::Immutable,
                    ),
                ),
            ],
            ty_return,
            body,
        )
    }
}

pub fn output_parser(
    cx: &ExtCtxt,
    span: Span,
    tm: &RuleTranslationMap,
    rules: &Vec<Rule>,
    terminals: &HashSet<(String, String)>,
    lalr_table: &LRTable,
) -> Box<MacResult + 'static> {
    let builder = AstBuilderCx {
        cx,
        span,
        tm,
        lalr_table,
        terminals,
    };

    let mut items: SmallVec<[P<ast::Item>; 1]> = rules
        .iter()
        .filter(|item| !(item.identifier == "Epsilon"))
        .flat_map(|item| builder.get_enum_item(item))
        .collect();

    let all_structs_enum = ast::EnumDef {
        variants: rules
            .iter()
            .map(|rule| builder.get_plain_variant(span, &rule.identifier))
            .chain(terminals.iter().map(|(name, _)| {
                cx.variant(
                    span,
                    cx.ident_of(name),
                    vec![cx.ty_path(cx.path(
                        span,
                        vec![
                            cx.ident_of("Token"),
                            //cx.ident_of(name),
                        ],
                    ))],
                )
            }))
            .collect(),
    };

    items.push(builder.item_enum_derive(
        span,
        cx.ident_of("Epsilon"),
        ast::EnumDef {
            variants: vec![cx.variant(span, cx.ident_of("Epsilon"), vec![])],
        },
    ));

    items.push(builder.item_enum_derive(span, cx.ident_of("_Data"), all_structs_enum));

    items.push(builder.get_lalr_table());
    items.push(builder.get_translation_fn());
    items.push(builder.get_translation_enum());
    items.push(builder.get_action_enum());
    items.push(builder.get_starter_var());
    items.push(builder.get_wrapper_fn(terminals));
    items.push(builder.get_reduction_fn(rules));

    let mut driver_fn = r#"
pub fn driver(tokenstream: &mut Iterator<Item = &Token>) -> Option<S> {
    let mut tokens = tokenstream.peekable();
    let mut stack: Vec<(usize, Box<_Data>)> =
        vec![(_STARTER_VALUE, Box::new(_Data::Epsilon(Epsilon::Epsilon)))];
    let debug_output = std::env::var("DEBUG_LALR_RUNTIME").is_ok();
    loop {
        let (s, _) = stack[stack.len() - 1];
        let a_ = tokens.peek().map(|x| *x);
        if debug_output {
            println!("Current state: {}", s);
            println!("Peeked token: {:?}", &a_);
        }
        let a = _convert_token_to_index(a_);
        if debug_output {
            println!("Peeked token index: {}", a);
        }
        let action = &_STATE_TABLE[s][a];
        if debug_output {
            println!("Action: {:?}", action);
        }
        match action {
            _Act::Error => panic!("Error state reached"),
            _Act::Shift(t) => {
                stack.push((*t, _token_to_data(a, a_.unwrap())));
                let t = tokens.next();
                if debug_output {
                    println!("Consumed token: {:?}", t);
                }
            }
            _Act::Reduce(goto, subrule, count) => {
                let rem_range = stack.len() - count..;
                let drain: Vec<Box<_Data>> = stack.drain(rem_range).map(|(_, t)| t).collect();
                let (t, _) = stack[stack.len() - 1];
                match _STATE_TABLE[t][*goto] {
                    _Act::Goto(g) => {
                        let result = _reduce_to_ast(*goto, *subrule, &drain);
                        stack.push((g, result));
                    }
                    _ => panic!("Unreachable"),
                }
            }
            _Act::Accept => {
                break;
            }
            _Act::Goto(_) => panic!("Unreachable"),
        }
    }

    let t = stack.into_iter().last().unwrap().1;
            "#
    .to_string();

    driver_fn.push_str(&format!(
        "
    if let box _Data::{0}(s) = t {{
        Some(S::{0}(S_{0}(s)))
    }} else {{
        None
    }} }}",
        tm.rules["S"].data[0].1[0].identifier
    ));

    let session = cx.parse_sess();
    let filename = FileName::Custom("lalr_driver_fn".to_string());
    items.push(parse::new_parser_from_source_str(session, filename, driver_fn).parse_item().unwrap().unwrap());

    return MacEager::items(items);
}
