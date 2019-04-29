use purkkasyntax::TypeSignature;

#[derive(Clone, Debug)]
pub struct ResolveResult {
    pub full_path: String,
    pub c_content: String,
    pub h_content: Option<String>,
    pub dependencies: Option<Vec<String>>,
    pub declarations: Option<Vec<(String, TypeSignature)>>,
}

#[derive(Clone, Debug)]
pub struct FileQuery {
    pub current_file: String,
    pub requested_file: String,
    pub local_file: bool,
    pub need_raw: bool,
}

impl FileQuery {
    pub fn new(current: &str, requested: &str, local: bool, raw: bool) -> FileQuery {
        FileQuery {
            current_file: current.to_string(),
            requested_file: requested.to_string(),
            local_file: local,
            need_raw: raw,
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
        }
    }

    pub fn is_ok(&self) -> bool {
        true
    }
}
