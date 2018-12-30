extern crate preprocessor;

fn process(original: &str, expected: &str) {
    assert_eq!(
        preprocessor::preprocess("foo.c", original),
        Ok(expected.to_string()));
}

#[test]
fn simple_macros() {
    process("#define FOO BAR\nFOO\n", "\nBAR\n");
 }
