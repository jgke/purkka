use smallvec::SmallVec;
use syntax::ast;
use syntax::ext::base::{ExtCtxt, MacEager, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::Span;
use syntax::source_map::respan;
use syntax::ptr::P;

use std::collections::HashSet;

use types::{LRTable, Rule, RuleTranslationMap};

fn get_enum_item(cx: &mut ExtCtxt, rule: &Rule) -> Vec<P<ast::Item>> {
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
                ty = cx.ty_path(
                    cx.path_all(item.span, true,
                                cx.std_path(&["boxed", "Box"]),
                                vec![ast::GenericArg::Type(ident_ty)],
                                Vec::new()));
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
            let mut total_span = components.get(0).unwrap().span;
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

//pub fn get_translation_fn(cx: &ExtCtxt, span: Span, tm: &RuleTranslationMap) -> P<ast::Item> {
//    let enum_name = &rule.identifier;
//    let enumdef = ast::EnumDef {
//        variants: tm.rules.iter().flat_map(|(name, rule)| {
//            cx.variant(span, cx.ident_of(format!("{}_{}", &name)), vals)
//        }).collect()
//    }
//
//    let ident = cx.ident_of(enum_name);
//    cx.item_enum(rule.span, ident, enumdef)
//}

pub fn output_parser(cx: &mut ExtCtxt, span: Span, tm: &RuleTranslationMap, items: &Vec<Rule>, terminals: &HashSet<(String, String)>, _lalr_table: &LRTable)
    -> Box<MacResult + 'static> {

        let mut items: SmallVec<[P<ast::Item>; 1]> = items.iter()
            .filter(|item| !(item.identifier == "Epsilon"))
            .flat_map(|item| get_enum_item(cx, item))
            .collect();
        items.push(cx.item_enum(span, cx.ident_of("Epsilon"), ast::EnumDef {
            variants: vec![cx.variant(span, cx.ident_of("E"), vec![])]
        }));

        println!("{:?} {:?}", tm.indices, terminals);

        items.push(get_translation_fn(cx, span, tm, terminals));
        //items.push(get_translation_enum(cx, span, tm));

        return MacEager::items(items);
    }
