use std::collections::HashMap;
use std::rc::Rc;

use purkkasyntax::*;

fn b_fn(syms: &mut (HashMap<Rc<str>, TypeSignature>, Vec<Rc<str>>),
        name: &str, params: Vec<&TypeSignature>, ret: &TypeSignature) {
    syms.0.insert(
        From::from(name),
        TypeSignature::Function(
            params.into_iter().cloned().map(Box::new).map(Param::TypeOnly).collect(),
            Box::new(ret.clone())));
}

pub fn insert_builtins(syms: &mut (HashMap<Rc<str>, TypeSignature>, Vec<Rc<str>>)) {
    let v2df = &TypeSignature::Vector(Primitive::Double);
    let int = &TypeSignature::int();
    let voidp = &TypeSignature::Pointer { ty: Box::new(TypeSignature::void()), nullable: false };
    let sizet = &TypeSignature::size_t();

    b_fn(syms, "__builtin_ia32_haddpd", vec![v2df, v2df], v2df);
    b_fn(syms, "__builtin_ia32_sqrtpd", vec![v2df], v2df);

    b_fn(syms, "__builtin_expect", vec![int, int], int);
    b_fn(syms, "__builtin_memset", vec![voidp, int, sizet], voidp);
}
