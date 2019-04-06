//! String interner.

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct StringInterner {
    interner: HashMap<String, Rc<str>>
}

impl StringInterner {
    pub fn new() -> StringInterner {
        StringInterner {
            interner: HashMap::new()
        }
    }

    pub fn get_ref(&mut self, name: &str) -> Rc<str> {
        if !self.interner.contains_key(name) {
            self.interner.insert(name.to_string(), From::from(name.to_string()));
        }
        Rc::clone(&self.interner[name])
    }
}

#[test]
fn simple_intern() {
    let mut interner = StringInterner::new();
    interner.get_ref("foo");
    assert_eq!(interner.interner.len(), 1);
    interner.get_ref("foo");
    interner.get_ref("foo");
    interner.get_ref("foo");
    assert_eq!(interner.interner.len(), 1);
    interner.get_ref("bar");
    assert_eq!(interner.interner.len(), 2);
    interner.get_ref("bar");
    assert_eq!(interner.interner.len(), 2);
}
