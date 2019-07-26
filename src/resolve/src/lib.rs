use std::collections::HashSet;
use std::rc::Rc;

use purkkasyntax::TypeSignature;

pub type Declarations = Vec<(Rc<str>, TypeSignature)>;

#[derive(Clone, Debug)]
pub struct ResolveResult {
    pub full_path: String,
    pub c_content: String,
    pub h_content: Option<String>,
    pub dependencies: Option<Vec<String>>,
    pub declarations: Option<Declarations>,
    pub types: Option<Declarations>,
    pub c_macros: (HashSet<Rc<str>>, HashSet<Rc<str>>),
}

#[derive(Clone, Debug)]
pub struct FileQuery {
    pub current_file: String,
    pub requested_file: String,
    pub local_file: bool,
    pub need_raw: bool,
    pub include_next: bool,
    pub types: HashSet<Rc<str>>,
}

impl FileQuery {
    pub fn new(current: &str, requested: &str, local: bool, raw: bool, types: HashSet<Rc<str>>) -> FileQuery {
        FileQuery {
            current_file: current.to_string(),
            requested_file: requested.to_string(),
            local_file: local,
            need_raw: raw,
            include_next: false,
            types,
        }
    }
    pub fn new_next(current: &str, requested: &str, local: bool, raw: bool, include_next: bool, types: HashSet<Rc<str>>) -> FileQuery {
        FileQuery {
            current_file: current.to_string(),
            requested_file: requested.to_string(),
            local_file: local,
            need_raw: raw,
            include_next,
            types,
        }
    }
}

impl ResolveResult {
    pub fn new_raw(path: &str, content: &str) -> Self {
        ResolveResult {
            full_path: path.to_string(),
            c_content: content.to_string(),
            h_content: None,
            dependencies: None,
            declarations: None,
            types: None,
            c_macros: (HashSet::new(), HashSet::new()),
        }
    }

    pub fn is_ok(&self) -> bool {
        true
    }
}
