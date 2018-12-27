#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

extern crate lalr_runtime;

use std::fmt;
use std::vec::Drain;

#[derive(Clone,Debug)]
enum Token {
    Constant(),
    Plus(),
    Minus(),
    Times(),
    //Divide(),
    OpenParen(),
    CloseParen(),
    //c(), d()
}

//lalr! {
//    S -> SS;
//    SS -> C C;
//    C -> #Token::c C | #Token::d;
//}

//lalr! {
//    S -> PlusExpr;
//    PlusExpr -> TimesExpr
//              | Plus. PlusExpr #Token::Plus TimesExpr
//              | Minus. PlusExpr #Token::Minus TimesExpr;
//    TimesExpr -> PrimaryExpr
//               | Times. TimesExpr #Token::Times PrimaryExpr
//               | Divide. TimesExpr #Token::Divide PrimaryExpr;
//    PrimaryExpr -> #Token::Constant
//                 | #Token::OpenParen &PlusExpr #Token::CloseParen;
//}

lalr! {
    S -> SS;
    SS -> L #Token::Plus R | R;
    L -> #Token::Minus R | #Token::Constant;
    R -> &L;
}

impl fmt::Display for _Act {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            _Act::Error => write!(f, "e   "),
            _Act::Shift(to) => write!(f, "s{: <3}", &to.to_string()),
            _Act::Reduce(rule, subrule, count) => write!(f, "r{},{}", rule, &subrule.to_string()),
            _Act::Accept => write!(f, "acc "),
            _Act::Goto(to) => write!(f, "g{: <3}", to),
        }
    }
}

fn driver(tokenstream: &mut Iterator<Item = &Token>) -> S {
    let mut w = tokenstream.peekable();
    let mut stack: Vec<(usize, Box<_Data>)> =
        vec![(_STARTER_VALUE, Box::new(_Data::Epsilon(Epsilon::Error)))];
    loop {
        println!("{:?}", stack);
        let (s, _) = stack[stack.len() - 1];
        let a_ = w.peek().map(|x| *x);
        let a = _convert_token_to_index(w.peek().map(|x| *x));
        let act_row = &_STATE_TABLE[s];
        let action = &act_row[a];
        match action {
            _Act::Error => panic!("Error"),
            _Act::Shift(t) => {
                stack.push((*t, _token_to_data(a, a_.unwrap())));
                w.next();
            }
            _Act::Reduce(goto, subrule, count) => {
                let rem_range = stack.len() - count..;
                let drain: Vec<Box<_Data>> = stack.drain(rem_range).map(|(_, t)| t).collect();
                let (t, _) = stack[stack.len() - 1];
                match _STATE_TABLE[t][*goto] {
                    _Act::Goto(g) => {
                        println!("{:?} {:?} {:?}", *goto, *subrule, &drain);
                        let result = _reduce_to_ast(*goto, *subrule, &drain);
                        stack.push((g, result));
                    }
                    _ => panic!("Unreachable"),
                }
                println!("Reduce {} ({})", goto, count);
            }
            _Act::Accept => {
                break;
            }
            _Act::Goto(i) => panic!("Unreachable"),
        }
    }

    println!("Accept");

    let t = stack.into_iter().last().unwrap().1;

    if let box _Data::SS(s) = t {
        S::SS(S_SS(s))
    } else {
        panic!("");
    }
}

//pub fn parse_tokens<'a>(tokens: &mut Iterator<Item = &TokenType>) -> TranslationUnit<'a> {
//    TranslationUnit::ExternalDeclaration(&[])
//}

#[test]
fn it_compiles() {}

#[test]
fn token_parsing() {
    use Token::*;

    for row in &_STATE_TABLE {
        for cell in row {
            print!("{} ", cell);
        }
        println!("");
    }

    for i in &[Constant(), Plus(), Times(), OpenParen(), CloseParen()] {
        println!("{:?} {}", i, _convert_token_to_index(Some(i)));
    }

    println!("{:?}", driver(&mut [Constant(), Plus(), Constant()].iter()));
}
