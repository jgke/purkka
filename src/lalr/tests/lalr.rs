#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

type State = ();

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Equals(),
    Star(),
    Id(),
}

//lalr! {
//    S  -> SS;
//    SS -> L #Token::Equals R
//        | R;
//    L  -> #Token::Star R
//        | #Token::Id;
//    R  -> &L;
//}
lalr! {
    S  -> SS;
    SS -> C C;
    C  -> #Token::Star C | #Token::Id;
}

#[test]
fn parse_lalr() {
    println!("1");
    assert_eq!(
        driver(&mut [Token::Id(), Token::Id()].iter(), &mut ()),
        Ok(S::SS(S_SS(SS::C(SS_C(C::Id(C_Id(Token::Id())), C::Id(C_Id(Token::Id())))))))
    );
}
