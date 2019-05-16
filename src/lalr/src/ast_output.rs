use smallvec::SmallVec;
use syntax::ast;
use syntax::ext::base::{ExtCtxt, MacEager, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::parse;
use syntax::ptr::P;
use syntax::source_map::{respan};
use syntax_pos::symbol;
use syntax_pos::{FileName, Span};

use std::collections::{HashMap, HashSet};

use crate::types::{Component, Index, Rule, RuleTranslationMap, Terminal};

pub fn first(
    tm: &RuleTranslationMap,
    set: &mut HashSet<Index>,
    cache: &mut HashSet<Index>,
    rule_index: Index,
) -> bool {
    if cache.get(&rule_index).is_some() {
        return false;
    }
    cache.insert(rule_index);
    let rule = &tm.rules.get(&rule_index).unwrap_or_else(|| {
        panic!(
            "No rule found for index {} ({:?})",
            rule_index,
            tm.rev_indices.get(&rule_index)
        )
    });
    let mut has_e = false;
    // E -> A | B
    for (i, _) in rule.data.iter().enumerate() {
        let mut sub_cache = cache.clone();
        has_e |= first_loop(tm, set, &mut sub_cache, (rule_index, i, 0));
    }
    has_e
}

fn first_loop(
    tm: &RuleTranslationMap,
    set: &mut HashSet<Index>,
    cache: &mut HashSet<Index>,
    rule: (Index, usize, usize),
) -> bool {
    let (rule_index, rule_subindex, position) = rule;
    let rule = &tm.rules[&rule_index];
    let Component { rules, .. } = &rule.data[rule_subindex];
    let mut has_e = true;
    // E -> A
    for (i, ruledata) in rules.iter().enumerate() {
        if i < position {
            continue;
        }
        // A
        if ruledata.identifier == "Epsilon" {
            has_e = true;
        } else if ruledata.terminal {
            set.insert(tm.indices[&ruledata.full_path]);
            has_e = false;
            break;
        } else {
            let sub_has_e = first(tm, set, cache, tm.indices[&ruledata.full_path]);
            if !sub_has_e {
                has_e = false;
                break;
            }
        }
    }
    has_e
}

struct AstBuilderCx<'a, 'c> {
    cx: &'a ExtCtxt<'c>,
    span: Span,
    tm: &'a RuleTranslationMap,
    special_rules: &'a HashMap<String, String>,
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
                                match self.special_rules.get(ident_str) {
                                    Some(_) => self.ty_ident("Token"),
                                    None => {
                                        if item.indirect || &item.identifier == enum_name {
                                            self.boxed(ident_ty)
                                        } else {
                                            ident_ty
                                        }
                                    }
                                }
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

    pub fn get_first_fn(&self, rules: &[Rule]) -> P<ast::Item> {
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
    rules: &[Rule],
    terminals: &HashSet<Terminal>,
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
        let list = special_provides.entry(dependant).or_insert_with(Vec::new);
        list.push((identifier, provider));
    }

    let builder = AstBuilderCx {
        cx,
        span,
        tm,
        special_rules: &special_rules,
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
    items.push(builder.get_first_fn(rules));

    MacEager::items(items)
}
