use smallvec::SmallVec;
use syntax::ast;
use syntax::ext::base::{ExtCtxt, MacEager, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::Span;
use syntax::source_map::{respan, Spanned};
use syntax::ptr::P;

use std::collections::HashSet;

use types::{Action, LRTable, Rule, RuleTranslationMap};

fn boxed(cx: &ExtCtxt, span: Span, ty: P<ast::Ty>) -> P<ast::Ty> {
    cx.ty_path(cx.path_all(span, true,
                           cx.std_path(&["boxed", "Box"]),
                           vec![ast::GenericArg::Type(ty)],
                           Vec::new()))
}

fn sliced(cx: &ExtCtxt, span: Span, ty: P<ast::Ty>, count: usize) -> P<ast::Ty> {
    cx.ty(span, ast::TyKind::Array(ty, ast::AnonConst {
                id: ast::DUMMY_NODE_ID,
                value: cx.expr_usize(span, count)
    }))
}

fn get_enum_item(cx: &ExtCtxt, rule: &Rule) -> Vec<P<ast::Item>> {
    let enum_name = &rule.identifier;
    let mut enum_items: Vec<P<ast::Item>> = rule.data.clone().into_iter().map(|(variant_identifier, components)| {
        let mut total_span = components.get(0).unwrap().span;
        let vals = components.into_iter().map(|item| {
            total_span = total_span.to(item.span);
            //let (n, x, s) = item;
            let ident_arr = item.full_path.rsplitn(2, "::");
            // .unwrap() here is always safe
            let ident_ty = cx.ty_ident(item.span, cx.ident_of(ident_arr.last().unwrap()));
            let ty;
            if item.indirect || &item.identifier == enum_name {
                ty = boxed(cx, item.span, ident_ty);
            } else {
                ty = ident_ty;
            }
            ast::StructField {
                span: item.span,
                ty,
                ident: None,
                vis: respan(item.span.shrink_to_lo(), ast::VisibilityKind::Inherited),
                attrs: Vec::new(),
                id: ast::DUMMY_NODE_ID
            }
        }).collect();
        cx.item_struct(
            total_span,
            cx.ident_of(&format!("{}_{}", rule.identifier, variant_identifier)),
            ast::VariantData::Tuple(vals, ast::DUMMY_NODE_ID)
        )
    }).collect();
    let enumdef = ast::EnumDef {
        variants: rule.data.clone().into_iter().map(|(variant_identifier, components)| {
            let var_id = cx.ident_of(&format!("{}_{}", rule.identifier, variant_identifier));
            let total_span = components.get(0).unwrap().span;
            cx.variant(total_span, cx.ident_of(&variant_identifier),
                       vec![cx.ty_ident(total_span, var_id)])
        }).collect()
    };

    let ident = cx.ident_of(enum_name);
    enum_items.push(cx.item_enum(rule.span, ident, enumdef));
    enum_items
}

fn terminal_pattern(cx: &ExtCtxt, span: Span, symbol: &String) -> P<ast::Pat> {
    cx.pat(span, ast::PatKind::TupleStruct(
            cx.path(span, symbol.split("::").map(|x| cx.ident_of(x)).collect()),
            vec![], Some(0)))
}

fn get_translation_fn_body(cx: &ExtCtxt, span: Span, tm: &RuleTranslationMap,
                           terminals: &HashSet<(String, String)>) -> P<ast::Block> {
    cx.block(span, vec![cx.stmt_expr(cx.expr_match(
                span,
                cx.expr_ident(span, cx.ident_of("token")),
                terminals.iter().map(
                    |(_, terminal)| cx.arm(
                        span,
                        vec![terminal_pattern(cx, span, &terminal)],
                        cx.expr_usize(span, tm.indices[terminal]))
                    ).collect()
                ))])
}

pub fn get_translation_fn(cx: &ExtCtxt, span: Span, tm: &RuleTranslationMap,
                          terminals: &HashSet<(String, String)>) -> P<ast::Item> {
    let ty_usize = cx.ty_ident(span, cx.ident_of("usize"));
    let terminal_base = &terminals.iter().next().unwrap().1.rsplitn(2, "::").last().unwrap();
    let ty_return = cx.ty_rptr(span,
                               cx.ty_ident(span, cx.ident_of(terminal_base)),
                               None,
                               ast::Mutability::Immutable);
    let body = get_translation_fn_body(cx, span, tm, terminals);
    cx.item_fn(span, cx.ident_of("_convert_token_to_index"),
    vec![cx.arg(span, cx.ident_of("token"), ty_return)],
    ty_usize,
    body)
}

pub fn get_translation_enum(cx: &ExtCtxt, span: Span, tm: &RuleTranslationMap) -> P<ast::Item> {
    let enumdef = ast::EnumDef {
        variants: tm.rules.iter().flat_map(|(name, rule)| {
            let tmp: Vec<_> = rule.data.iter().map(
                |(variant, components)|  {
                    let var_id = cx.ident_of(&format!("{}_{}", name, variant));
                    let span = components.get(0).unwrap().span;
                    cx.variant(span, var_id, vec![cx.ty_ident(span, var_id)])
                }
            ).collect();
            tmp
        }).collect()
    };

    let ident = cx.ident_of("_lalr_symbols");
    cx.item_enum(span, ident, enumdef)
}

pub fn enum_variant(cx: &ExtCtxt, span: Span, name: &str, args: Vec<&str>) -> Spanned<ast::Variant_> {
    cx.variant(span, cx.ident_of(name), args.iter().map(|x| cx.ty_ident(span, cx.ident_of(x))).collect())
}

pub fn get_action_enum(cx: &ExtCtxt, span: Span) -> P<ast::Item> {
    let enumdef = ast::EnumDef {
        variants: vec![
            enum_variant(cx, span, "E", vec![]),
            enum_variant(cx, span, "S", vec!["usize"]),
            enum_variant(cx, span, "R", vec!["usize", "usize"]),
            enum_variant(cx, span, "A", vec![]),
            enum_variant(cx, span, "G", vec!["usize"]),
        ]
    };

    let ident = cx.ident_of("_Act");
    cx.item_enum(span, ident, enumdef)
}

fn action_expr(cx: &ExtCtxt, span: Span, args: (&str, Option<Vec<P<ast::Expr>>>)) -> P<ast::Expr> {
    let act = cx.path(span, vec![cx.ident_of("_Act"), cx.ident_of(args.0)]);
    match args.1 {
        Some(expr) => cx.expr_call(span, cx.expr_path(act), expr),
        None => cx.expr_path(act)
    }
}

fn usize_expr(cx: &ExtCtxt, span: Span, i: usize) -> P<ast::Expr> {
    cx.expr_lit(span, ast::LitKind::Int(i as u128, ast::LitIntType::Unsuffixed))
}

fn lalr_action_call(cx: &ExtCtxt, span: Span, tm: &RuleTranslationMap, action: &Action) -> P<ast::Expr> {
    action_expr(cx, span,
                match action {
                    Action::Error => ("E", None),
                    Action::Shift(pat) => ("S", Some(vec![usize_expr(cx, span, *pat)])),
                    Action::Reduce(ident, a) => ("R", Some(vec![
                                                  cx.expr_usize(span, tm.indices[ident]),
                                                  cx.expr_usize(span, *a),
                    ])),
                    Action::Accept => ("A", None),
                    Action::Goto(pat) => ("G", Some(vec![cx.expr_usize(span, *pat)])),
                   //  => ("Error", vec![])
                }
                )
}

fn lalr_table_row(cx: &ExtCtxt, span: Span, tm: &RuleTranslationMap, actions: &Vec<Action>) -> P<ast::Expr> {
    cx.expr_vec(span, actions.iter()
                .map(|act| lalr_action_call(cx, span, tm, act))
                .collect())
}

fn get_full_lalr_table(tm: &RuleTranslationMap, lalr_table: &LRTable) -> Vec<Vec<Action>> {
    let mut rows: Vec<(&String, usize)> = tm.indices.iter().map(|(name, index)| (name, *index)).collect();
    rows.sort_unstable_by(|(_, index1), (_, index2)| index1.cmp(index2));
    lalr_table.actions.iter().enumerate()
        .map(|(index, _)| rows.iter()
             .map(|(lookahead, _)|
                  lalr_table.clone().actions
                  .get(index).unwrap()
                  .get(*lookahead).unwrap_or(&Action::Error))
             .map(|x| x.clone())
             .collect())
        .collect()
}

pub fn get_lalr_table_fn(cx: &ExtCtxt, span: Span, tm: &RuleTranslationMap,
                         lalr_table: &LRTable) -> P<ast::Item> {
    let act_ty = cx.ty_ident(span, cx.ident_of("_Act"));
    let count = tm.indices.len();
    let ty_return = sliced(cx, span, sliced(cx, span, act_ty, count), lalr_table.actions.len());
    let content = get_full_lalr_table(tm, lalr_table);
    cx.item_static(span,
                   cx.ident_of("_STATE_TABLE"),
                   ty_return,
                   ast::Mutability::Immutable,
                   cx.expr_vec(span, content.iter().map(|x| lalr_table_row(cx, span, tm, x)).collect()))
}

pub fn output_parser(cx: &ExtCtxt, span: Span, tm: &RuleTranslationMap, items: &Vec<Rule>, terminals: &HashSet<(String, String)>, lalr_table: &LRTable)
    -> Box<MacResult + 'static> {
        let mut items: SmallVec<[P<ast::Item>; 1]> = items.iter()
            .filter(|item| !(item.identifier == "Epsilon"))
            .flat_map(|item| get_enum_item(cx, item))
            .collect();
        items.push(cx.item_enum(span, cx.ident_of("Epsilon"), ast::EnumDef {
            variants: vec![cx.variant(span, cx.ident_of("E"), vec![])]
        }));

        items.push(get_translation_fn(cx, span, tm, terminals));
        items.push(get_translation_enum(cx, span, tm));
        items.push(get_action_enum(cx, span));
        items.push(get_lalr_table_fn(cx, span, tm, lalr_table));

        return MacEager::items(items);
    }
