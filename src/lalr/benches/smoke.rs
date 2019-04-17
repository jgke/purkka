#![feature(plugin, box_patterns)]
#![feature(test)]
#![plugin(lalr)]
#![allow(non_camel_case_types)]

extern crate test;

use test::Bencher;

type State = ();

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Constant(),
}

lalr! {
    S -> A;
    A -> B B B B;
    B -> C C C C;
    C -> D D D D;
    D -> E E E E;
    E -> F F F F;
    F -> G G G G;
    G -> H H H H;
    H -> #Token::Constant;
}

#[bench]
fn bench_reductions(b: &mut Bencher) {
    use Token::*;

    b.iter(|| {
        let tree = driver(
            &mut std::iter::repeat(Constant())
                .take(16384)
                .collect::<Vec<_>>()
                .iter(),
            &mut (),
        );
        assert!(tree.is_ok());
        tree
    });
}
