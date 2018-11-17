#![feature(plugin)]
#![plugin(lalr)]
#![allow(dead_code)]

lalr! {
    Foo -> Bar Baz;
    Boo -> Bar Baz;
}

#[test]
fn it_compiles() {
}
