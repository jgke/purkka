#![feature(plugin)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

use std::fmt;

#[derive(Debug)]
enum Token {
    Constant(),
    Plus(),
    //Minus(),
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
    S -> E;
    E -> E #Token::Plus T
       | T;
    T -> T #Token::Times F
       | F;
    F -> #Token::OpenParen &E #Token::CloseParen
       | #Token::Constant;
}

impl fmt::Display for _Act {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            _Act::Error => write!(f, "e   "),
            _Act::Shift(to) => write!(f, "s{: <3}", &to.to_string()),
            _Act::Reduce(rule, subrule) => {
                write!(f, "r{},{}", rule, &subrule.to_string())
            }
            _Act::Accept => write!(f, "acc "),
            _Act::Goto(to) => write!(f, "g{: <3}", to),
        }
    }
}

fn driver(tokenstream: &mut Iterator<Item=&Token>) -> S {
    let mut w = tokenstream.peekable();
    let mut stack: Vec<usize> = vec![_STARTER_VALUE];
    loop {
        let s = stack[stack.len()-1];
        let a = _convert_token_to_index(w.peek().map(|x| *x));
        let act_row = &_STATE_TABLE[s];
        let action = &act_row[a];
        match action {
            _Act::Error => panic!("Error"),
            _Act::Shift(t) => {
                stack.push(*t);
                w.next();
            }
            _Act::Reduce(goto, count) => {
                let rem_range = stack.len()-count..;
                stack.drain(rem_range);
                let t = stack[stack.len()-1];
                match _STATE_TABLE[t][*goto] {
                    _Act::Goto(g) => stack.push(g),
                    _ => panic!("Unreachable")
                }
                println!("Reduce {} ({})", goto, count);
            }
            _Act::Accept => {
                break;
            },
            _Act::Goto(i) => panic!("Unreachable"),
        }
    }

    println!("Accept");

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

    driver(&mut [
           Constant(), Plus(), Constant()
    ].iter());
}
