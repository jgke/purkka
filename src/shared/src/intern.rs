//! Generic HashMap-based interner.

use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

/// Simple HashMap-based interner.
#[derive(Debug, Default)]
pub struct Interner<K, V>
where
    K: Clone + Hash + Eq,
    V: ?Sized,
{
    interner: HashMap<K, Rc<V>>,
}

impl<K, V> Clone for Interner<K, V>
where
    K: Clone + Hash + Eq,
    V: ?Sized,
{
    fn clone(&self) -> Self {
        Self {
            interner: self.interner.clone(),
        }
    }
}

impl<K, V> Interner<K, V>
where
    K: Clone + Hash + Eq,
    V: ?Sized,
{
    pub fn new() -> Interner<K, V> {
        Interner {
            interner: HashMap::new(),
        }
    }

    pub fn get_ref<'a, B: ?Sized>(&mut self, name: &'a B) -> Rc<V>
    where
        K: Borrow<B> + From<&'a B>,
        Rc<V>: From<&'a B>,
        B: Hash + Eq,
    {
        if !self.interner.contains_key(name) {
            self.interner.insert(From::from(name), From::from(name));
        }
        Rc::clone(&self.interner[name])
    }
}

pub type StringInterner = Interner<String, str>;

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

    // quick test for derived traits
    println!("{:?}", interner.clone());
}
