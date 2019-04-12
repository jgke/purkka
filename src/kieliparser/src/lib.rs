#![recursion_limit="100"]

#![feature(plugin, box_patterns)]
#![plugin(lalr)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

pub mod parser;

use parser::*;

pub fn it_compiles() {
}
