use smallvec::SmallVec;
use syntax::ast;
use syntax::ext::base::{ExtCtxt, MacEager, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::ext::quote::rt::Span;
use syntax::ptr::P;

use std::collections::HashSet;

use types::{LRTable, Rule, RuleTranslationMap};

fn get_enum_item(cx: &mut ExtCtxt, rule: &Rule) -> P<ast::Item> {
    let enum_name = &rule.identifier;
    let enumdef = ast::EnumDef {
        variants: rule.data.clone().into_iter().map(|(variant_identifier, components)| {
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
                ty
            }).collect();
            cx.variant(total_span, cx.ident_of(&variant_identifier), vals)
        }).collect()
    };

    let ident = cx.ident_of(enum_name);
    cx.item_enum(rule.span, ident, enumdef)
}

pub fn output_parser(cx: &mut ExtCtxt, span: Span, _tm: &RuleTranslationMap, items: &Vec<Rule>, _terminals: &HashSet<(String, String)>, _lalr_table: &LRTable)
    -> Box<MacResult + 'static> {

    let mut items: SmallVec<[P<ast::Item>; 1]> = items.iter()
        .filter(|item| !(item.identifier == "Epsilon"))
        .map(|item| get_enum_item(cx, item))
        .collect();
    items.push(cx.item_enum(span, cx.ident_of("Epsilon"), ast::EnumDef {
        variants: vec![cx.variant(span, cx.ident_of("E"), vec![])]
    }));

    return MacEager::items(items);
}
