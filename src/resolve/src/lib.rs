use purkkatypes::TypeSignature;

#[derive(Clone, Debug)]
pub struct ParseResult {
    pub c_content: String,
    pub h_content: Option<String>,
    pub dependencies: Option<Vec<String>>,
    pub declarations: Option<Vec<(String, TypeSignature)>>,
}

#[derive(Clone, Debug)]
pub struct FileQuery {
    pub current_file: String,
    pub requested_file: String,
    pub need_raw: bool
}
