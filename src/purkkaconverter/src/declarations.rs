/// Fetch all declarations from the file
use purkkasyntax::visitor::*;
use purkkasyntax::*;

#[derive(Debug)]
pub struct FetchDeclarations {
    pub declarations: Vec<Declaration>,
    pub types: Vec<Typedef>,
}

impl FetchDeclarations {
    pub fn new() -> FetchDeclarations {
        FetchDeclarations {
            declarations: Vec::new(),
            types: Vec::new(),
        }
    }
    pub fn fetch_declarations(&mut self, s: &mut S) {
        self.visit_s(s);
    }
}

impl ASTVisitor for FetchDeclarations {
    fn visit_declaration(&mut self, e: &mut Declaration) {
        match e {
            Declaration::Declaration(..) => self.declarations.push(e.clone()),
        }
    }
    fn visit_typedef(&mut self, e: &mut Typedef) {
        match e {
            Typedef::Struct(..) => self.types.push(e.clone()),
            Typedef::Enum(..) => self.types.push(e.clone()),
            Typedef::Alias(..) => {}
        }
    }
}
