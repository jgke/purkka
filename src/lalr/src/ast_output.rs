use rustc_target::spec::abi::Abi;
use smallvec::SmallVec;
use syntax::ast;
use syntax::ext::base::{ExtCtxt, MacEager, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::parse;
use syntax::ptr::P;
use syntax::source_map::{dummy_spanned, respan, Spanned};
use syntax_pos::symbol;
use syntax_pos::{FileName, Span};

use std::collections::{HashMap, HashSet};
use std::iter;

use crate::generator::first;
use crate::types::{Action, Component, LRTable, Rule, RuleData, RuleTranslationMap, Terminal};

struct AstBuilderCx<'a, 'c> {
    cx: &'a ExtCtxt<'c>,
    span: Span,
    tm: &'a RuleTranslationMap,
    lalr_table: Option<&'a LRTable>,
    terminals: &'a HashSet<Terminal>,
    special_rules: &'a HashMap<String, String>,
    special_provides: &'a HashMap<String, Vec<(String, String)>>,
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

    fn get_enum_item(&self, rule: &Rule) -> P<ast::Item> {
        let enum_name = &rule.identifier;
        let enumdef = ast::EnumDef {
            variants: rule
                .data
                .clone()
                .into_iter()
                .map(
                    |Component {
                         real_name, rules, ..
                     }| {
                        let total_span = rules.get(0).unwrap().span;

                        let vals = rules
                            .clone()
                            .into_iter()
                            .filter(|r| r.identifier != "Epsilon")
                            .map(|item| {
                                let ident_arr = item.full_path.rsplitn(2, "::");
                                let ident_str = ident_arr.last().unwrap();
                                let ident_ty = self.ty_ident(ident_str);
                                let ty = match self.special_rules.get(ident_str) {
                                    Some(_) => self.ty_ident("Token"),
                                    None => {
                                        if item.indirect || &item.identifier == enum_name {
                                            self.boxed(ident_ty)
                                        } else {
                                            ident_ty
                                        }
                                    }
                                };
                                ty
                            })
                            .collect::<Vec<_>>();
                        let is_empty = vals.is_empty();
                        let mut variant =
                            self.cx
                                .variant(total_span, self.cx.ident_of(&real_name), vals);
                        if is_empty {
                            variant.node.data =
                                ast::VariantData::Tuple(Vec::new(), ast::DUMMY_NODE_ID);
                        }
                        variant
                    },
                )
                .collect(),
        };

        let ident = self.cx.ident_of(enum_name);
        self.item_enum_derive(rule.span, ident, enumdef)
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

    pub fn get_debug_translation_fn(&self) -> P<ast::Item> {
        let ty_return = self.cx.ty_rptr(
            self.span,
            self.ty_ident("str"),
            Some(self.cx.lifetime(self.span, self.cx.ident_of("'static"))),
            ast::Mutability::Immutable,
        );
        let name = self.cx.ident_of("_token_index_to_str");
        let inputs = vec![self.cx.arg(
            self.span,
            self.cx.ident_of("token"),
            self.cx.ty_ident(self.span, self.cx.ident_of("usize")),
        )];
        let output = ty_return;

        let body = self.cx.block_expr(
            self.cx.expr_match(
                self.span,
                self.cx.expr_ident(self.span, self.cx.ident_of("token")),
                self.tm
                    .rules
                    .iter()
                    .map(|(_, rule)| &rule.identifier)
                    .chain(self.terminals.iter().map(|term| &term.full_path))
                    .map(|name| {
                        let span = self
                            .tm
                            .indices
                            .get(name)
                            .and_then(|index| self.tm.rules.get(index))
                            .map(|rule| rule.span)
                            .unwrap_or(self.span);
                        self.cx.arm(
                            span,
                            vec![self
                                .cx
                                .pat_lit(span, self.usize_expr(self.tm.indices[name]))],
                            self.cx.expr_str(span, symbol::Symbol::intern(name)),
                        )
                    })
                    .chain(iter::once(self.wild_arm_unreachable(
                        "Tried to convert unknown token index to string: {}",
                        vec!["token"],
                    )))
                    .collect(),
            ),
        );

        self.cx.item(
            self.span,
            name,
            vec![],
            ast::ItemKind::Fn(
                self.cx.fn_decl(inputs, ast::FunctionRetTy::Ty(output)),
                ast::FnHeader {
                    unsafety: ast::Unsafety::Normal,
                    asyncness: dummy_spanned(ast::IsAsync::NotAsync),
                    constness: dummy_spanned(ast::Constness::NotConst),
                    abi: Abi::Rust,
                },
                ast::Generics::default(),
                body,
            ),
        )
    }
    pub fn get_debug_reduce_translation_fn(&self) -> P<ast::Item> {
        let ty_return = self.cx.ty_rptr(
            self.span,
            self.ty_ident("str"),
            Some(self.cx.lifetime(self.span, self.cx.ident_of("'static"))),
            ast::Mutability::Immutable,
        );
        let name = self.cx.ident_of("_reduce_index_to_str");
        let inputs = vec![
            self.cx.arg(
                self.span,
                self.cx.ident_of("index"),
                self.cx.ty_ident(self.span, self.cx.ident_of("usize")),
            ),
            self.cx.arg(
                self.span,
                self.cx.ident_of("subindex"),
                self.cx.ty_ident(self.span, self.cx.ident_of("usize")),
            ),
        ];
        let output = ty_return;

        let body = self.cx.block_expr(
            self.cx.expr_match(
                self.span,
                self.cx.expr_tuple(
                    self.span,
                    vec![
                        self.cx.expr_ident(self.span, self.cx.ident_of("index")),
                        self.cx.expr_ident(self.span, self.cx.ident_of("subindex")),
                    ],
                ),
                self.tm
                    .rules
                    .iter()
                    .flat_map(|(_, rule)| {
                        let index = self.tm.indices[&rule.identifier];
                        rule.data.iter().enumerate().map(move |(i, data)| {
                            (index, i, data.real_name.clone(), rule.span.clone())
                        })
                    })
                    .map(|(index, subindex, name, span)| {
                        self.cx.arm(
                            span,
                            vec![self.cx.pat_tuple(
                                span,
                                vec![
                                    self.cx.pat_lit(self.span, self.usize_expr(index)),
                                    self.cx.pat_lit(self.span, self.usize_expr(subindex)),
                                ],
                            )],
                            self.cx.expr_str(span, symbol::Symbol::intern(&name)),
                        )
                    })
                    .chain(iter::once(self.wild_arm_unreachable(
                        "Tried to convert unknown (index, subindex) to str: {} {}",
                        vec!["index", "subindex"],
                    )))
                    .collect(),
            ),
        );

        self.cx.item(
            self.span,
            name,
            vec![],
            ast::ItemKind::Fn(
                self.cx.fn_decl(inputs, ast::FunctionRetTy::Ty(output)),
                ast::FnHeader {
                    unsafety: ast::Unsafety::Normal,
                    asyncness: dummy_spanned(ast::IsAsync::NotAsync),
                    constness: dummy_spanned(ast::Constness::NotConst),
                    abi: Abi::Rust,
                },
                ast::Generics::default(),
                body,
            ),
        )
    }

    fn translation_fn_maybe_with_conversion(&self, term: &Terminal) -> P<ast::Expr> {
        let default_case = self
            .cx
            .expr_usize(term.span, self.tm.indices[&term.full_path]);
        let token_unwrap = self.cx.expr_method_call(
            term.span,
            self.cx.expr_ident(term.span, self.cx.ident_of("token")),
            self.cx.ident_of("unwrap"),
            vec![],
        );
        match self.special_provides.get(&term.full_path) {
            Some(functions) => {
                functions
                    .iter()
                    .rev()
                    .fold(default_case, |default_case, (actual, function)| {
                        if self.tm.indices.get(actual).is_none() {
                            println!("Warning: unused special case: {}", actual);
                            return default_case;
                        }
                        let is_special = self.cx.expr_call_ident(
                            term.span,
                            self.cx.ident_of(function),
                            vec![
                                self.cx.expr_ident(term.span, self.cx.ident_of("_state")),
                                token_unwrap.clone(),
                            ],
                        );
                        let special_case = self.cx.expr_usize(term.span, self.tm.indices[actual]);
                        self.cx
                            .expr_if(term.span, is_special, special_case, Some(default_case))
                    })
            }
            None => default_case,
        }
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
                            .filter(|term| term.conversion_fn.is_none())
                            .map(|term| {
                                self.cx.arm(
                                    self.span,
                                    vec![self.terminal_pattern(&term.full_path)],
                                    self.translation_fn_maybe_with_conversion(&term),
                                )
                            })
                            .chain(iter::once(self.wild_arm_unreachable(
                                "Tried to convert unknown token to index\n{:?}",
                                vec!["token"],
                            )))
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
            .filter(|term| term.conversion_fn.is_none())
            .next()
            .unwrap()
            .full_path
            .rsplitn(2, "::")
            .last()
            .unwrap();
        let ty_return = self.cx.ty_rptr(
            self.span,
            self.ty_ident(terminal_base),
            None,
            ast::Mutability::Immutable,
        );
        let ty_state = self.cx.ty_rptr(
            self.span,
            self.ty_ident("State"),
            None,
            ast::Mutability::Mutable,
        );
        let body = self.get_translation_fn_body();
        let span = self.span;
        let name = self.cx.ident_of("_convert_token_to_index");
        let inputs = vec![
            self.cx.arg(
                self.span,
                self.cx.ident_of("token"),
                self.cx.ty_option(ty_return),
            ),
            self.cx.arg(self.span, self.cx.ident_of("_state"), ty_state),
        ];
        let output = ty_usize;

        // #[allow(unreachable_patterns)]
        let unreachable_patterns = self.cx.attribute(
            self.span,
            self.cx.meta_list(
                self.span,
                symbol::Symbol::intern("allow"),
                vec![self.cx.meta_list_item_word(
                    self.span,
                    symbol::Symbol::intern("unreachable_patterns"),
                )],
            ),
        );

        self.cx.item(
            span,
            name,
            vec![unreachable_patterns],
            ast::ItemKind::Fn(
                self.cx.fn_decl(inputs, ast::FunctionRetTy::Ty(output)),
                ast::FnHeader {
                    unsafety: ast::Unsafety::Normal,
                    asyncness: dummy_spanned(ast::IsAsync::NotAsync),
                    constness: dummy_spanned(ast::Constness::NotConst),
                    abi: Abi::Rust,
                },
                ast::Generics::default(),
                body,
            ),
        )
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
            Action::Shift(pat, _prio) => ("Shift", Some(vec![self.usize_expr(*pat)])),
            Action::Reduce(ident, subrule, count, _prio) => (
                "Reduce",
                Some(vec![
                    self.usize_expr(*ident),
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
            .unwrap()
            .actions
            .iter()
            .enumerate()
            .map(|(index, _)| {
                rows.iter()
                    .map(|(lookahead, _)| {
                        self.lalr_table
                            .unwrap()
                            .clone()
                            .actions
                            .get(index)
                            .unwrap()
                            .get(&self.tm.indices[*lookahead])
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
        let ty_return = self.sliced(
            self.sliced(act_ty, count),
            self.lalr_table.unwrap().actions.len(),
        );
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

    fn get_wrapper_fn_body(&self, terminals: &HashSet<Terminal>) -> P<ast::Block> {
        self.cx.block(
            self.span,
            vec![self.cx.stmt_expr(
                // match index {
                self.cx.expr_match(
                    self.span,
                    self.cx.expr_ident(self.span, self.cx.ident_of("index")),
                    terminals
                        .iter()
                        .filter(|term| self.tm.indices.contains_key(&term.full_path))
                        .map(|term| {
                            self.cx.arm(
                                self.span,
                                vec![self.cx.pat_lit(
                                    self.span,
                                    self.usize_expr(self.tm.indices[&term.full_path]),
                                )],
                                self.box_expr(self.cx.expr_call(
                                    self.span,
                                    self.cx.expr_path(self.cx.path(
                                        self.span,
                                        vec![
                                            self.cx.ident_of("_Data"),
                                            self.cx.ident_of(&term.identifier),
                                        ],
                                    )),
                                    vec![self.box_or_star(
                                        false,
                                        self.cx.expr_ident(self.span, self.cx.ident_of("token")),
                                    )],
                                )),
                            )
                        })
                        .chain(iter::once(self.wild_arm_unreachable(
                            "Tried to convert unknown index to _Data: {}\n{:?}",
                            vec!["index", "token"],
                        )))
                        .collect(),
                ),
                // }
            )],
        )
    }
    pub fn get_wrapper_fn(&self, terminals: &HashSet<Terminal>) -> P<ast::Item> {
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

    fn reduction_match_data(&self, subindex: usize, data: &Vec<RuleData>) -> P<ast::Pat> {
        let slice_pat: P<ast::Pat> = self.cx.pat(
            self.span,
            ast::PatKind::Slice(
                data.iter()
                    .enumerate()
                    .filter(|(_, ruledata)| ruledata.identifier != "Epsilon")
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
        let unreachable_expr = format!(
            "{{ panic!(\"{{}}, {{}} {{}} - got {{:?}}\", index, subindex, \"{}\", data) }}",
            func
        );
        let expr = parse::new_parser_from_source_str(session, filename, unreachable_expr)
            .parse_expr()
            .unwrap();
        self.cx
            .arm(self.span, vec![self.cx.pat_wild(self.span)], expr)
    }
    fn wild_arm_unreachable(&self, msg: &str, args: Vec<&str>) -> ast::Arm {
        let session = self.cx.parse_sess();
        let filename = FileName::Custom(format!("lalr_driver_fn_unreachable_{}", msg));
        let unreachable_expr = format!("{{ panic!(\"{}\", {})}}", msg, args.join(","));
        let expr = parse::new_parser_from_source_str(session, filename, unreachable_expr)
            .parse_expr()
            .unwrap();
        self.cx
            .arm(self.span, vec![self.cx.pat_wild(self.span)], expr)
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
    fn reduction_match_body(&self, rule: &Rule) -> P<ast::Expr> {
        let Rule {
            identifier,
            span,
            data,
            ..
        } = rule;
        let bodies = data.iter().enumerate().map(
            |(
                subindex,
                Component {
                    real_name,
                    rules,
                    action,
                    priority: _priority,
                },
            )| {
                if rules.len() == 1 && rules[0].identifier == "Epsilon" && false {
                    self.cx.arm(
                        *span,
                        vec![self.cx.pat_wild(*span)],
                        self.box_expr(self.cx.expr_call(
                            *span,
                            self.cx.expr_path(self.cx.path(
                                *span,
                                vec![self.cx.ident_of("_Data"), self.cx.ident_of("Epsilon")],
                            )),
                            vec![self.cx.expr_path(self.cx.path(
                                *span,
                                vec![self.cx.ident_of("Epsilon"), self.cx.ident_of("Epsilon")],
                            ))],
                        )),
                    )
                } else {
                    let args: Vec<P<ast::Expr>> = rules
                        .iter()
                        .enumerate()
                        .filter(|(_, data)| data.identifier != "Epsilon")
                        .map(|(i, data)| {
                            self.box_or_star(
                                data.indirect,
                                self.cx
                                    .expr_ident(*span, self.cx.ident_of(&format!("pat_{}", i))),
                            )
                        })
                        .collect();
                    let result = self.box_expr(self.cx.expr_call(
                        *span,
                        self.cx.expr_path(self.cx.path(
                            *span,
                            vec![self.cx.ident_of("_Data"), self.cx.ident_of(identifier)],
                        )),
                        vec![self.cx.expr_call(
                            *span,
                            self.cx.expr_path(self.cx.path(
                                *span,
                                vec![self.cx.ident_of(identifier), self.cx.ident_of(real_name)],
                            )),
                            args.clone(),
                        )],
                    ));

                    match action {
                        Some(callback) => self.cx.arm(
                            *span,
                            vec![self.reduction_match_data(subindex, rules)],
                            self.cx.expr_block(self.cx.block(
                                *span,
                                vec![
                                            self.cx.stmt_expr(
                                                self.cx.expr_call_ident(
                                                    *span,
                                                    self.cx.ident_of(callback),
                                                    iter::once(self.cx.expr_ident(
                                                        *span,
                                                        self.cx.ident_of("_state"),
                                                    ))
                                                    .chain(args.into_iter())
                                                    .collect(),
                                                ),
                                            ),
                                            self.cx.stmt_expr(result),
                                        ],
                            )),
                        ),
                        None => self.cx.arm(
                            *span,
                            vec![self.reduction_match_data(subindex, rules)],
                            result,
                        ),
                    }
                }
            },
        );

        self.cx.expr_match(
            *span,
            self.cx.expr_tuple(
                *span,
                vec![
                    self.cx.expr_ident(*span, self.cx.ident_of("subindex")),
                    self.cx.expr_ident(*span, self.cx.ident_of("data")),
                ],
            ),
            bodies
                .chain(iter::once(self.wild_arm_fail("coerce_panic")))
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
                                rule.span,
                                vec![self.cx.pat_lit(
                                    rule.span,
                                    self.usize_expr(self.tm.indices[&rule.identifier]),
                                )],
                                self.reduction_match_body(&rule),
                            )
                        })
                        .chain(iter::once(self.wild_arm_unreachable(
                            "Tried to convert unknown rule to AST\n{:?} {:?} {:?}",
                            vec!["index", "subindex", "data"],
                        )))
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
                self.cx.arg(
                    self.span,
                    self.cx.ident_of("_state"),
                    self.cx.ty_rptr(
                        self.span,
                        self.ty_ident("State"),
                        None,
                        ast::Mutability::Mutable,
                    ),
                ),
            ],
            ty_return,
            body,
        )
    }
    pub fn get_first_fn(&self, rules: &Vec<Rule>) -> P<ast::Item> {
        let header = r#"
    #[macro_export]
    macro_rules! match_first {
        ($val:expr => $bind:ident, default $else:expr, $($body:tt)* ) => {
            match_first!(@rules $val, $bind, $else, {}, $($body)* )
        };
        "#;

        let single_match_start = r#"(@rules $val:expr, $bind:ident, $else:expr, {$($arms:tt)*}, "#;
        let single_match_middle = r#" => $e:expr, $($tail:tt)*) => {
            match_first!(@rules $val, $bind, $else, {
                $($arms)* "#;
        let single_match_tail = r#"
            },
            $($tail)*)
        };
        "#;

        let tail = r#"
        (@rules $val:expr, $bind:ident, $else:expr, {$($arms:tt)*}, $(,)*) => {
            match $val {
                $($arms)*
                $bind => { $else }
            }
        };
   }
        "#;

        let mut result = header.to_string();
        for rule in rules.iter().filter(|t| t.identifier != "$") {
            let mut single_match = single_match_start.to_string();
            single_match += &rule.identifier;
            single_match += single_match_middle;
            let mut first_terms = HashSet::new();
            let _has_e = first(
                self.tm,
                &mut first_terms,
                &mut HashSet::new(),
                self.tm.indices[&rule.identifier],
            );
            single_match += &first_terms
                .iter()
                .map(|t| format!("Some($bind @ {}(..))", self.tm.rev_indices[t]))
                .collect::<Vec<String>>()
                .join("|");
            single_match += " => { $e }";
            single_match += single_match_tail;
            result += &single_match;
        }
        result += tail;

        let session = self.cx.parse_sess();
        let filename = FileName::Custom("lalr_first_macro".to_string());
        let tmp = parse::new_parser_from_source_str(session, filename, result).parse_item();
        tmp.unwrap().unwrap()
    }
}

pub fn output_parser(
    cx: &ExtCtxt,
    span: Span,
    tm: &RuleTranslationMap,
    rules: &Vec<Rule>,
    terminals: &HashSet<Terminal>,
    lalr_table: Option<&LRTable>,
) -> Box<MacResult + 'static> {
    let special_rules: HashMap<String, String> = terminals
        .iter()
        .filter(|term| term.conversion_fn.is_some())
        .map(|term| {
            let identifier = term.identifier.clone();
            let dependant = term.conversion_fn.as_ref().unwrap().1.clone();
            (identifier, dependant)
        })
        .collect();

    let mut special_provides: HashMap<String, Vec<(String, String)>> = HashMap::new();

    for term in terminals {
        if term.conversion_fn.is_none() {
            continue;
        }

        let identifier = term.identifier.clone();
        let dependant = term.conversion_fn.as_ref().unwrap().1.clone();
        let provider = term.conversion_fn.as_ref().unwrap().0.clone();
        let list = special_provides.entry(dependant).or_insert(Vec::new());
        list.push((identifier, provider));
    }

    let builder = AstBuilderCx {
        cx,
        span,
        tm,
        lalr_table,
        terminals,
        special_rules: &special_rules,
        special_provides: &special_provides,
    };

    let mut items: SmallVec<[P<ast::Item>; 1]> = rules
        .iter()
        .filter(|item| item.identifier != "Epsilon")
        .filter(|item| item.identifier != "$")
        .map(|item| {
            item.enumdef
                .clone()
                .unwrap_or_else(|| builder.get_enum_item(item))
        })
        .collect();

    let all_structs_enum = ast::EnumDef {
        variants: rules
            .iter()
            .filter(|item| item.identifier != "$")
            .map(|rule| builder.get_plain_variant(span, &rule.identifier))
            .chain(terminals.iter().map(|term| {
                cx.variant(
                    span,
                    cx.ident_of(&term.identifier),
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

    let mut driver_fn = r#"
pub fn driver(tokenstream: &mut Iterator<Item = &Token>, state: &mut State) -> Result<S, Option<Token>> {
    let mut tokens = tokenstream.peekable();
    let mut stack: Vec<(usize, Box<_Data>)> =
        vec![(_STARTER_VALUE, Box::new(_Data::Epsilon(Epsilon::Epsilon)))];
    let debug_output = std::env::var("DEBUG_LALR_RUNTIME").is_ok();
    loop {
        let (s, _) = stack[stack.len() - 1];
        let a_ = tokens.peek().map(|x| *x);
        if debug_output {
            println!("\nPeeked token: {:?}", &a_);
        }
        let a = _convert_token_to_index(a_, state);
        let action = &_STATE_TABLE[s][a];
        if debug_output {
            println!("Action: {:?}", action);
        }
        match action {
            _Act::Error => {
                println!("Error state reached for token {:?}", &a_);
                println!("Expected one of tokens:");
                &_STATE_TABLE[s].iter().enumerate()
                    .filter(|(_, t)| if let _Act::Shift(..) = t { true } else { false })
                    .for_each(|(i, _)| println!("{}", _token_index_to_str(i)));
                println!("Alternatively, would reduce for tokens:");
                &_STATE_TABLE[s].iter().enumerate()
                    .filter(|(_, t)| if let _Act::Reduce(..) = t { true } else { false })
                    .for_each(|(i, _)| println!("{}", _token_index_to_str(i)));
                return Err(a_.map(|t| t.clone()))
            }
            _Act::Shift(t) => {
                stack.push((*t, _token_to_data(a, a_.unwrap())));
                tokens.next();
            }
            _Act::Reduce(goto, subrule, count) => {
                let rem_range = stack.len() - count..;
                let drain: Vec<Box<_Data>> = stack.drain(rem_range).map(|(_, t)| t).collect();
                let (t, _) = stack[stack.len() - 1];
                match _STATE_TABLE[t][*goto] {
                    _Act::Goto(g) => {
                        let result = _reduce_to_ast(*goto, *subrule, &drain, state);
                        if debug_output {
                            println!("Reduced rule: {:?}[{}]", _reduce_index_to_str(*goto, *subrule), *subrule);
                        }
                        stack.push((g, result));
                    }
                    _ => panic!("Non-Goto action in Goto table"),
                }
            }
            _Act::Accept => {
                break;
            }
            _Act::Goto(_) => panic!("Goto action in Shift/Reduce table"),
        }
    }

    let t = stack.into_iter().last().unwrap().1;
            "#
    .to_string();

    driver_fn.push_str(&format!(
        "
    if let box _Data::{0}(s) = t {{
        Ok(S::{0}(s))
    }} else {{
        Err(None)
    }} }}",
        tm.rules[&tm.indices["S"]].data[0].rules[0].identifier
    ));

    if lalr_table.is_some() {
        items.push(builder.get_lalr_table());
        items.push(builder.get_debug_translation_fn());
        items.push(builder.get_debug_reduce_translation_fn());
        items.push(builder.get_translation_fn());
        items.push(builder.get_action_enum());
        items.push(builder.get_starter_var());
        items.push(builder.get_wrapper_fn(terminals));
        items.push(builder.get_reduction_fn(rules));

        let session = cx.parse_sess();
        let filename = FileName::Custom("lalr_driver_fn".to_string());
        items.push(
            parse::new_parser_from_source_str(session, filename, driver_fn)
                .parse_item()
                .unwrap()
                .unwrap(),
        );
    } else {
        items.push(builder.get_first_fn(rules));
    }

    return MacEager::items(items);
}
