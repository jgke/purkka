use std::convert::TryFrom;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeSignature {
    Plain(Rc<str>),
    Pointer { nullable: bool, ty: Box<TypeSignature> },

    Struct(Option<Rc<str>>, Vec<StructField>),
    Enum(Option<Rc<str>>, Vec<EnumField>),
    Tuple(Vec<Box<TypeSignature>>),
    Array(Box<TypeSignature>, Option<usize>),

    Function(Vec<Param>, Box<TypeSignature>),
    Infer
}

#[derive(Clone, Debug, PartialEq)]
pub enum Param {
    Param(Rc<str>, Box<TypeSignature>),
    Anon(Box<TypeSignature>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructField {
    Field { name: Rc<str>, ty: Box<TypeSignature> }
}

#[derive(Clone, Debug, PartialEq)]
pub enum EnumField {
    Field { name: Rc<str>, value: Option<i128>, ty: Option<TypeSignature> }
}

impl TryFrom<Param> for TypeSignature {
    type Error = ();

    fn try_from(param: Param) -> Result<Self, Self::Error> {
        match param {
            Param::Anon(ty) => Ok(*ty),
            Param::Param(..) => Err(()),
        }
    }
}
