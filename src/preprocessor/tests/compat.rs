mod common;

use common::*;

#[test]
fn asm_expr() {
    assert_eq!(
        preprocess_string("main.c", "#define FOO a ## b ## c\nFOO",)
            .unwrap()
            .len(),
        1
    );
    assert_eq!(
        preprocess_string("main.c", "#define FOO(t) a ## b ## c\nFOO(_)",)
            .unwrap()
            .len(),
        1
    );
    assert_eq!(
        preprocess_string("main.c", "#define FOO(a) a ## b ## c\nFOO(a)",)
            .unwrap()
            .len(),
        1
    );
    assert_eq!(
        preprocess_string("main.c", "#define FOO bar /* baz */ \\\nqux\n",)
            .unwrap()
            .len(),
        0
    );
    assert_eq!(
        preprocess_string("main.c", "#define FOO bar /* baz */\\\nqux\n",)
            .unwrap()
            .len(),
        0
    );
    assert_eq!(
        preprocess_string("main.c", "#if !defined(FOO)\nbar\n#endif\n",)
            .unwrap()
            .len(),
        1
    );
}
