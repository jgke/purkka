extern crate preprocessor;
extern crate shared;
extern crate ctoken;

mod common;

use common::*;

#[test]
fn asm_expr() {
    assert_eq!(preprocess_string(
        "main.c",
        "asm ( anything can go here [ ] ( ) )",
    ).unwrap().len(), 1);
    assert_eq!(preprocess_string(
        "main.c",
        "asm volatile inline goto ( anything can go here [ ] ( ) )",
    ).unwrap().len(), 1);
}
