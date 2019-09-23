/// Fetch all declarations from the file
use purkkasyntax::visitor::*;
use purkkasyntax::*;

#[derive(Debug)]
pub struct FetchDeclarations {
    pub declarations: Vec<Declaration>,
    pub types: Vec<Typedef>,
}

#[allow(unused_must_use)]
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
    unit_result!();
    type Err = ();
    fn visit_declaration(&mut self, e: &mut Declaration) -> Result<(), ()> {
        match e {
            Declaration::Declaration(..) => self.declarations.push(e.clone()),
        }
        Ok(())
    }
    fn visit_typedef(&mut self, e: &mut Typedef) -> Result<(), ()> {
        self.types.push(e.clone());
        Ok(())
    }
}
