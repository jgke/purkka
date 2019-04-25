/// Fetch all declarations from the file
use std::collections::HashMap;
use std::rc::Rc;

use purkkaparser::parser::*;
use purkkaparser::visitor::*;

#[derive(Debug)]
pub struct FetchDeclarations {
    pub declarations: HashMap<Rc<str>, Declaration>,
}

impl FetchDeclarations {
    pub fn new() -> FetchDeclarations {
        FetchDeclarations {
            declarations: HashMap::new(),
        }
    }
    pub fn fetch_declarations(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for FetchDeclarations {
    fn visit_declaration(&mut self, e: &mut Declaration) {
        match e {
            Declaration::Declaration(_, _, name, _, _) => {
                self.declarations.insert(name.clone(), e.clone());
            }
        }
    }
}
