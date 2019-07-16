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
}

#[derive(Clone, Debug)]
pub struct FileQuery {
    pub current_file: String,
    pub requested_file: String,
    pub local_file: bool,
    pub need_raw: bool,
    pub include_next: bool,
}

impl FileQuery {
    pub fn new(current: &str, requested: &str, local: bool, raw: bool) -> FileQuery {
        FileQuery {
            current_file: current.to_string(),
            requested_file: requested.to_string(),
            local_file: local,
            need_raw: raw,
            include_next: false
        }
    }
    pub fn new_next(current: &str, requested: &str, local: bool, raw: bool, include_next: bool) -> FileQuery {
        FileQuery {
            current_file: current.to_string(),
            requested_file: requested.to_string(),
            local_file: local,
            need_raw: raw,
            include_next
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
        }
    }

    pub fn is_ok(&self) -> bool {
        true
    }
}
