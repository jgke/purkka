#![feature(plugin)]
#![plugin(lalr)]
#![allow(dead_code)]

enum Token {
    Foo, Bar
}

lalr! {
    Baz -> #Token::Bar;
    Foo -> Bar;
    Boo -> Bar Baz;
    Bar -> &Foo;
}

#[test]
fn it_compiles() {
}
