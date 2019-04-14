//! Generic HashMap-based interner.

use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;
use std::borrow::Borrow;


/// Simple HashMap-based interner.
pub struct Interner<K, V>
where K: Hash + Eq,
      V: ?Sized {
    interner: HashMap<K, Rc<V>>
}

impl<K, V> Interner<K, V>
where K: Hash + Eq,
      V: ?Sized {
    pub fn new() -> Interner<K, V> {
        Interner {
            interner: HashMap::new()
        }
    }

    pub fn get_ref<'a, B: ?Sized>(&mut self, name: &'a B) -> Rc<V>
        where K: Borrow<B> + From<&'a B>,
              Rc<V>: From<&'a B>,
              B: Hash + Eq
    {
        if !self.interner.contains_key(name) {
            self.interner.insert(From::from(name), From::from(name));
        }
        Rc::clone(&self.interner[name])
    }
}

type StringInterner = Interner<String, str>;

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
