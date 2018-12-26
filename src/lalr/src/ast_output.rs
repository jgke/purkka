use smallvec::SmallVec;
use syntax::ast;
use syntax::ext::base::{ExtCtxt, MacEager, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::Span;
use syntax::ptr::P;
use syntax::source_map::{respan, Spanned};

use std::collections::HashSet;

use types::{Action, LRTable, Rule, RuleTranslationMap};

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
                self.cx.item_struct(
                    total_span,
                    self.cx
                        .ident_of(&format!("{}_{}", rule.identifier, variant_identifier)),
                    ast::VariantData::Tuple(vals, ast::DUMMY_NODE_ID),
                )
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
        enum_items.push(self.cx.item_enum(rule.span, ident, enumdef));
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
        self.cx.block(
            self.span,
            vec![self.cx.stmt_expr(
                self.cx.expr_match(
                    self.span,
                    self.cx.expr_ident(self.span, self.cx.ident_of("token")),
                    self.terminals
                        .iter()
                        .map(|(_, terminal)| {
                            self.cx.arm(
                                self.span,
                                vec![self.terminal_pattern(&terminal)],
                                self.cx.expr_usize(self.span, self.tm.indices[terminal]),
                            )
                        })
                        .collect(),
                ),
            )],
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
        self.cx.item_fn(
            self.span,
            self.cx.ident_of("_convert_token_to_index"),
            vec![self.cx.arg(self.span, self.cx.ident_of("token"), ty_return)],
            ty_usize,
            body,
        )
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
        self.cx.item_enum(self.span, ident, enumdef)
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
                self.enum_variant("E", vec![]),
                self.enum_variant("S", vec!["usize"]),
                self.enum_variant("R", vec!["usize", "usize"]),
                self.enum_variant("A", vec![]),
                self.enum_variant("G", vec!["usize"]),
            ],
        };

        let ident = self.cx.ident_of("_Act");
        self.cx.item_enum(self.span, ident, enumdef)
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
            Action::Error => ("E", None),
            Action::Shift(pat) => ("S", Some(vec![self.usize_expr(*pat)])),
            Action::Reduce(ident, a) => (
                "R",
                Some(vec![
                    self.usize_expr(self.tm.indices[ident]),
                    self.usize_expr(*a),
                ]),
            ),
            Action::Accept => ("A", None),
            Action::Goto(pat) => ("G", Some(vec![self.usize_expr(*pat)])),
            //  => ("Error", vec![])
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

    pub fn get_lalr_table_fn(&self) -> P<ast::Item> {
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
}

pub fn output_parser(
    cx: &ExtCtxt,
    span: Span,
    tm: &RuleTranslationMap,
    items: &Vec<Rule>,
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

    let mut items: SmallVec<[P<ast::Item>; 1]> = items
        .iter()
        .filter(|item| !(item.identifier == "Epsilon"))
        .flat_map(|item| builder.get_enum_item(item))
        .collect();
    items.push(cx.item_enum(
        span,
        cx.ident_of("Epsilon"),
        ast::EnumDef {
            variants: vec![cx.variant(span, cx.ident_of("E"), vec![])],
        },
    ));

    items.push(builder.get_translation_fn());
    items.push(builder.get_translation_enum());
    items.push(builder.get_action_enum());
    items.push(builder.get_lalr_table_fn());

    return MacEager::items(items);
}
