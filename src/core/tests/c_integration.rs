use preprocessor::PreprocessorOptions;

use core::core::get_file_cb;

use resolve::*;

fn parse(content: &str) -> ResolveResult {
    let input = "main.c";
    let get_file_content = |req: &FileQuery| {
        if req.requested_file == input {
            (content.to_string(), req.requested_file.clone())
        } else {
            panic!("Unexpected include: {}", req.requested_file)
        }
    };

    let options = PreprocessorOptions {
        include_path: vec![],
        include_files: vec![],
        definitions: vec![],
    };

    let res = get_file_cb(&options, &get_file_content)(FileQuery::new(".", input, true, false));
    println!("{}", res.c_content);
    res
}

#[test]
fn fn_with_block_inside() {
    assert!(parse(
        "
int main() {
    ({
    });
}
"
    )
    .is_ok());
}

#[test]
fn initialization_expression() {
    assert!(parse(
        "
int a = (1);
"
    )
    .is_ok());
}

#[test]
fn asm_weirdness() {
    assert!(parse(
        "
#define ASM_MACRO asm ();
int main() {
    ASM_MACRO
}
"
    )
    .is_ok());
}

#[test]
fn va_types() {
    assert!(parse(
        "
int main() {
    int a;
    __builtin_va_list foo;
}
"
    )
    .is_ok());
}

#[test]
fn init_array() {
    assert!(parse(" int main() { int a[] = {}; } ").is_ok());

    assert!(parse(" int main() { int a[] = {1}; } ").is_ok());
    assert!(parse(" int main() { int a[] = {1,2}; } ").is_ok());
    assert!(parse(" int main() { int a[] = {1,2,3}; } ").is_ok());

    assert!(parse(" int main() { int a[] = {1,}; } ").is_ok());
    assert!(parse(" int main() { int a[] = {1,2,}; } ").is_ok());
    assert!(parse(" int main() { int a[] = {1,2,3,}; } ").is_ok());
}

#[test]
fn tons_of_types() {
    assert_eq!(parse("int a;").c_content, "int a;\n");
    assert_eq!(parse("long int a;").c_content, "long a;\n");
    assert_eq!(parse("long long int a;").c_content, "long long a;\n");
    assert_eq!(parse("long a;").c_content, "long a;\n");
    assert_eq!(parse("long double a;").c_content, "long double a;\n");
    assert_eq!(parse("enum foo { a };").c_content, "enum foo {\n    a\n};\n");
    assert_eq!(parse("enum foo { a, b };").c_content, "enum foo {\n    a,\n    b\n};\n");
    assert_eq!(parse("enum foo { a, b, c };").c_content, "enum foo {\n    a,\n    b,\n    c\n};\n");
    assert_eq!(parse("enum foo { a, };").c_content, "enum foo {\n    a\n};\n");
    assert_eq!(parse("enum foo { a, b, };").c_content, "enum foo {\n    a,\n    b\n};\n");
    assert_eq!(parse("enum foo { a, b, c, };").c_content, "enum foo {\n    a,\n    b,\n    c\n};\n");
    assert_eq!(parse("enum foo { a = 2, b, c, };").c_content, "enum foo {\n    a = 2,\n    b,\n    c\n};\n");
    assert_eq!(parse("enum foo { a = 1 + 2 };").c_content, "enum foo {\n    a = 1 + 2\n};\n");
    assert_eq!(parse("struct foo { int a; };").c_content, "struct foo {\n    int a;\n};\n");
    assert_eq!(parse("struct foo { int; };").c_content, "struct foo {\n    int;\n};\n");
    assert_eq!(parse("struct foo { int a; char b; };").c_content, "struct foo {\n    int a;\n    char b;\n};\n");
    assert_eq!(parse("struct foo { int a, b; };").c_content, "struct foo {\n    int a, b;\n};\n");
    assert_eq!(parse("struct foo { int *a, *b; };").c_content, "struct foo {\n    int *a, *b;\n};\n");
    assert_eq!(parse("struct foo { int *a, b; };").c_content, "struct foo {\n    int *a, b;\n};\n");
    assert_eq!(parse("struct foo { int *a, **b; };").c_content, "struct foo {\n    int *a, **b;\n};\n");
    assert_eq!(parse("struct foo { int a:8; };").c_content, "struct foo {\n    int a:8;\n};\n");
    assert_eq!(parse("struct foo { int a:8, b:4+1; };").c_content, "struct foo {\n    int a:8, b:4 + 1;\n};\n");

    assert_eq!(parse("union foo { int a; };").c_content, "union foo {\n    int a;\n};\n");
    assert_eq!(parse("union foo { int; };").c_content, "union foo {\n    int;\n};\n");
    assert_eq!(parse("union foo { int a; char b; };").c_content, "union foo {\n    int a;\n    char b;\n};\n");
    assert_eq!(parse("union foo { int a, b; };").c_content, "union foo {\n    int a, b;\n};\n");
    assert_eq!(parse("union foo { int *a, *b; };").c_content, "union foo {\n    int *a, *b;\n};\n");
    assert_eq!(parse("union foo { int *a, b; };").c_content, "union foo {\n    int *a, b;\n};\n");
    assert_eq!(parse("union foo { int *a, **b; };").c_content, "union foo {\n    int *a, **b;\n};\n");

    assert_eq!(parse("extern const unsigned char * const *ideal_nops;").c_content, "extern const unsigned char * const *ideal_nops;\n");

    assert_eq!(parse("int *foo;").c_content, "int *foo;\n");
    assert_eq!(parse("int (*fp)(int a);").c_content, "int (*fp)(int a);\n");
    assert_eq!(parse("int (*fp)(int);").c_content, "int (*fp)(int);\n");
}

#[test]
fn statements() {
    assert!(parse("int main() {
        if(1) { foo(); }
        if(2) bar(); else qux();
        switch(a) {
            case 1: foo(); bar();
            case 2: baz(); qux(); break;
            case 3 ... 4: baz(); qux(); break;
            default: panic();
        }
    }").is_ok());
    assert!(parse("int main() {
        continue;
        break;
        return;
        return 0;
        goto foo;
    }").is_ok());
    assert!(parse("int main() {
        while(1) foo();
        do foo(); while (bar);
        for (i = 0; i < 5; i++) { baz(); }
        for (i = 0; i < 5;) { baz(); }
        for (;;) { baz(); }
    }").is_ok());
    assert!(parse("int foo(...) {}").is_ok());
    assert!(parse("int foo(int a, ...) {}").is_ok());
    assert!(parse("int foo(int a, int b, ...) {}").is_ok());
}

#[test]
fn expressions() {
    assert!(parse("int a = 1 + 2 + 3;").is_ok());
    assert!(parse("int a = (int)1;").is_ok());
    assert!(parse("int *a = (int *)1;").is_ok());
    assert!(parse("int *a = foo();").is_ok());
    assert!(parse("int *a = foo(1);").is_ok());
    assert!(parse("int *a = foo(1 + 2);").is_ok());
    assert!(parse("int *a = foo(1 + 2, 3);").is_ok());
    assert!(parse("int a = a->b;").is_ok());
    assert!(parse("int a = a.b;").is_ok());
    assert!(parse("int a = a[b];").is_ok());
    assert!(parse("int a = b ? c : d;").is_ok());
}

#[test]
fn declarations() {
    assert!(parse("void foo(int);").is_ok());
    assert!(parse("void foo(void);").is_ok());
    assert!(parse("void foo(char **);").is_ok());
}

#[test]
fn typedefs() {
    assert!(parse("typedef int foo; foo a = 1;").is_ok());
}

#[test]
fn extensions() {
    assert!(parse("int a = asm(any tokens here);").is_ok());
}
