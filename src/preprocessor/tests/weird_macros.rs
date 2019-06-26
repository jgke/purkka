mod common;

use common::*;
use debug::debug::DEBUG_VALS;
use preprocessor::macrotoken::MacroTokenType;

fn equals_str(src: &str, res: &str) {
    println!("\n----------------------------\nSource file: {}\n----------------------------\n", src);
    let toks = preprocess_string("main.c", src).unwrap();

    // turn off debugs while parsing the expected result
    let list = DEBUG_VALS.iter().map(|t| (t, std::env::var(t).unwrap())).collect::<Vec<_>>();
    for s in DEBUG_VALS {
        std::env::set_var(s, "0");
    }
    let res_toks = preprocess_string("main.c", res).unwrap();
    for (t, v) in list {
        std::env::set_var(t, v);
    }

    println!("\n\nleft=expected, right=got");
    assert_eq!(
        res_toks.into_iter()
        .map(|t| t.ty)
        .map(|t| if let MacroTokenType::Identifier(ident, _) = t {
            MacroTokenType::Identifier(ident, false)
        } else {
            t
        })
        .collect::<Vec<_>>(),
        toks.into_iter()
        .map(|t| t.ty)
        .map(|t| if let MacroTokenType::Identifier(ident, _) = t {
            MacroTokenType::Identifier(ident, false)
        } else {
            t
        })
        .collect::<Vec<_>>(),
    );
}

/* These tests are taken from http://jhnet.co.uk/articles/cpp_magic */
#[test]
fn humble_define() {
    equals_str(
        r#"
    #define VERSION 123
    printf("Version: %d\n", VERSION);
    "#, r#"printf("Version: %d\n", 123);"#);
    equals_str(
        r#"
    #define MULTIPLY(a, b) ((a) * (b))
    printf("%d\n", MULTIPLY(4 + 2, 2 + 8) * 2);
    "#, r#"printf("%d\n", ((4 + 2) * (2 + 8)) * 2);"#);
    equals_str(
        r#"
    #define DEBUG(...) fprintf(stderr, __VA_ARGS__)
    DEBUG("Something went wrong in iteration: %d", i);
    "#, r#"fprintf(stderr, "Something went wrong in iteration: %d", i);"#);
}

#[test]
fn pattern_matching() {
    equals_str(
        r#"
#define IF_ELSE(condition) _IF_ ## condition
#define _IF_1(...) __VA_ARGS__ _IF_1_ELSE
#define _IF_0(...)             _IF_0_ELSE

#define _IF_1_ELSE(...)
#define _IF_0_ELSE(...) __VA_ARGS__
IF_ELSE(1)(it was one)(it was zero)
    "#,
    r#"it was one"#);
    equals_str(
        r#"
#define IF_ELSE(condition) _IF_ ## condition
#define _IF_1(...) __VA_ARGS__ _IF_1_ELSE
#define _IF_0(...)             _IF_0_ELSE

#define _IF_1_ELSE(...)
#define _IF_0_ELSE(...) __VA_ARGS__
IF_ELSE(0)(it was one)(it was zero)
    "#,
    r#"it was zero"#);
}

#[test]
fn cast_to_bool_1() {
    equals_str(
        r#"
    #define SECOND(a, b, ...) b
    #define IS_PROBE(...) SECOND(__VA_ARGS__, 0)
    #define PROBE() ~, 1
    SECOND(1,2,3)
    IS_PROBE(123)
    IS_PROBE(PROBE())
    "#, r#"2 0 1"#);
}

#[test]
fn cast_to_bool_2() {
    equals_str(
        r#"
#define SECOND(a, b, ...) b
#define IS_PROBE(...) SECOND(__VA_ARGS__, 0)
#define PROBE() ~, 1
#define CAT(a,b) a ## b
#define NOT(x) IS_PROBE(CAT(_NOT_, x))
#define _NOT_0 PROBE()
#define BOOL(x) NOT(NOT(x))

BOOL(0)
BOOL(1)
BOOL(123)
BOOL(not zero)
BOOL()

    "#, r#"0 1 1 1 1"#);
}

#[test]
fn cast_to_bool_3() {
    equals_str(
        r#"
#define SECOND(a, b, ...) b

#define IS_PROBE(...) SECOND(__VA_ARGS__, 0)
#define PROBE() ~, 1

#define CAT(a,b) a ## b

#define NOT(x) IS_PROBE(CAT(_NOT_, x))
#define _NOT_0 PROBE()

#define BOOL(x) NOT(NOT(x))

#define IF_ELSE(condition) _IF_ELSE(BOOL(condition))
#define _IF_ELSE(condition) CAT(_IF_, condition)

#define _IF_1(...) __VA_ARGS__ _IF_1_ELSE
#define _IF_0(...)             _IF_0_ELSE

#define _IF_1_ELSE(...)
#define _IF_0_ELSE(...) __VA_ARGS__

IF_ELSE(0)(it was non-zero)(it was zero)
IF_ELSE(1)(it was non-zero)(it was zero)
IF_ELSE(123)(it was non-zero)(it was zero)
    "#, r#"it was zero it was non-zero it was non-zero"#);
}

#[test]
fn iterators() {
    equals_str("#define RECURSIVE() I am RECURSIVE()\nRECURSIVE()", "I am RECURSIVE()");
}

#[test]
fn forcing_cpp_to_make_multiple_passes() {
    equals_str(
        r#"
    #define EMPTY()
    #define A(n) I like the number n
    
    A (123)
    A EMPTY() (123)
    "#, "I like the number 123 A (123)");
    equals_str(
        r#"
#define EMPTY()
#define A(n) I like the number n

#define EVAL1(...) __VA_ARGS__

A EMPTY() (123)
    "#, "A (123)");
    equals_str(
        r#"
#define EMPTY()
#define A(n) I like the number n

#define EVAL1(...) __VA_ARGS__

EVAL1(A EMPTY() (123))
    "#, "I like the number 123");
}

#[test]
fn turning_multiple_expansion_passes_to_recursion_0() {
    equals_str(
        r#"
#define EMPTY()

#define DEFER1(m) m EMPTY()

I am recursive, look: DEFER1(_RECURSE)()()
"#, r#"
I am recursive, look: _RECURSE ()()
"#);
}

#[test]
fn turning_multiple_expansion_passes_to_recursion_1() {
    equals_str(
        r#"
#define EMPTY()

#define DEFER1(m) m EMPTY()

#define RECURSE() I am recursive, look: DEFER1(_RECURSE)()()
#define _RECURSE() RECURSE

No EVAL: RECURSE()
"#, r#"
No EVAL: I am recursive, look: _RECURSE ()()
"#);
}

#[test]
fn turning_multiple_expansion_passes_to_recursion_2() {
    equals_str(
        r#"
#define EMPTY()

#define EVAL1(...) __VA_ARGS__

#define DEFER1(m) m EMPTY()

#define RECURSE() I am recursive, look: DEFER1(_RECURSE)()()
#define _RECURSE() RECURSE

With EVAL1: EVAL1(RECURSE())
"#, r#"
With EVAL1: I am recursive, look: I am recursive, look: _RECURSE ()()
"#);
}

#[test]
fn turning_multiple_expansion_passes_to_recursion_3() {
    equals_str(
        r#"
#define EMPTY()

#define EVAL32(...) EVAL16(EVAL16(__VA_ARGS__))
#define EVAL16(...) EVAL8(EVAL8(__VA_ARGS__))
#define EVAL8(...) EVAL4(EVAL4(__VA_ARGS__))
#define EVAL4(...) EVAL2(EVAL2(__VA_ARGS__))
#define EVAL2(...) EVAL1(EVAL1(__VA_ARGS__))
#define EVAL1(...) __VA_ARGS__

#define DEFER1(m) m EMPTY()

#define RECURSE() I am recursive, look: DEFER1(_RECURSE)()()
#define _RECURSE() RECURSE

With EVAL32: EVAL32(RECURSE())
"#, r#"
With EVAL32: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: I am recursive, look: _RECURSE ()()
"#);
}

#[test]
fn turning_recursion_into_an_iterator_1() {
    equals_str(
        r#"
#define EMPTY()

#define EVAL(...) EVAL1024(__VA_ARGS__)
#define EVAL1024(...) EVAL512(EVAL512(__VA_ARGS__))
#define EVAL512(...) EVAL256(EVAL256(__VA_ARGS__))
#define EVAL256(...) EVAL128(EVAL128(__VA_ARGS__))
#define EVAL128(...) EVAL64(EVAL64(__VA_ARGS__))
#define EVAL64(...) EVAL32(EVAL32(__VA_ARGS__))
#define EVAL32(...) EVAL16(EVAL16(__VA_ARGS__))
#define EVAL16(...) EVAL8(EVAL8(__VA_ARGS__))
#define EVAL8(...) EVAL4(EVAL4(__VA_ARGS__))
#define EVAL4(...) EVAL2(EVAL2(__VA_ARGS__))
#define EVAL2(...) EVAL1(EVAL1(__VA_ARGS__))
#define EVAL1(...) __VA_ARGS__

#define DEFER1(m) m EMPTY()

#define RECURSE() I am recursive, look: DEFER1(_RECURSE)()()
#define _RECURSE() RECURSE

#define MAP(m, first, ...) m(first) DEFER1(_MAP)()(m, __VA_ARGS__)
#define _MAP() MAP


#define GREET(x) Hello, x!
EVAL8(MAP(GREET, Mum, Dad, Adam, Joe))
"#, r#"Hello, Mum! Hello, Dad! Hello, Adam! Hello, Joe! Hello, ! Hello, ! Hello, ! Hello, ! Hello, ! Hello, ! Hello, ! Hello, ! Hello, ! Hello, ! Hello, ! Hello, ! _MAP ()(GREET, )"#);
}

#[test]
fn turning_recursion_into_an_iterator_2() {
    equals_str(
        r#"
#define FIRST(a, ...) a
#define SECOND(a, b, ...) b

#define IS_PROBE(...) SECOND(__VA_ARGS__, 0)
#define PROBE() ~, 1

#define CAT(a,b) a ## b

#define NOT(x) IS_PROBE(CAT(_NOT_, x))
#define _NOT_0 PROBE()

#define BOOL(x) NOT(NOT(x))

#define HAS_ARGS(...) BOOL(FIRST(_END_OF_ARGUMENTS_ __VA_ARGS__)())
#define _END_OF_ARGUMENTS_() 0

HAS_ARGS()
BOOL(FIRST(_END_OF_ARGUMENTS_)())
BOOL(_END_OF_ARGUMENTS_())
BOOL(0)

No args: HAS_ARGS()
One arg: HAS_ARGS(0)
Two args: HAS_ARGS(0, 2)
Three args: HAS_ARGS(0, 1, 2)
"#, r#"0 0 0 0 No args: 0 One arg: 1 Two args: 1 Three args: 1"#);
}

#[test]
fn turning_recursion_into_an_iterator_2_2() {
    equals_str(
        r#"
#define FIRST(a, ...) a
#define SECOND(a, b, ...) b

#define IS_PROBE(...) SECOND(__VA_ARGS__, 0)
#define PROBE() ~, 1

#define CAT(a,b) a ## b

#define NOT(x) IS_PROBE(CAT(_NOT_, x))
#define _NOT_0 PROBE()

#define BOOL(x) NOT(NOT(x))

#define HAS_ARGS(...) BOOL(FIRST(_END_OF_ARGUMENTS_ __VA_ARGS__)())
#define _END_OF_ARGUMENTS_() 0

HAS_ARGS()
"#, "0");
}

#[test]
fn turning_recursion_into_an_iterator_3_2() {
    equals_str(
        r#"
#define EVAL(...) __VA_ARGS__

#define MAP(m, first, ...) MAP(m, __VA_ARGS__)

#define FOO(x) x
EVAL(MAP(FOO, A, B))
"#, r#"MAP(FOO, B)"#);
}

#[test]
fn turning_recursion_into_an_iterator_3() {
    equals_str(
        r#"
#define FIRST(a, ...) a
#define SECOND(a, b, ...) b

#define EMPTY()

#define EVAL(...) EVAL2(__VA_ARGS__)
#define EVAL1024(...) EVAL512(EVAL512(__VA_ARGS__))
#define EVAL512(...) EVAL256(EVAL256(__VA_ARGS__))
#define EVAL256(...) EVAL128(EVAL128(__VA_ARGS__))
#define EVAL128(...) EVAL64(EVAL64(__VA_ARGS__))
#define EVAL64(...) EVAL32(EVAL32(__VA_ARGS__))
#define EVAL32(...) EVAL16(EVAL16(__VA_ARGS__))
#define EVAL16(...) EVAL8(EVAL8(__VA_ARGS__))
#define EVAL8(...) EVAL4(EVAL4(__VA_ARGS__))
#define EVAL4(...) EVAL2(EVAL2(__VA_ARGS__))
#define EVAL2(...) EVAL1(EVAL1(__VA_ARGS__))
#define EVAL1(...) __VA_ARGS__

#define DEFER1(m) m EMPTY()

#define IS_PROBE(...) SECOND(__VA_ARGS__, 0)
#define PROBE() ~, 1

#define CAT(a,b) a ## b

#define NOT(x) IS_PROBE(CAT(_NOT_, x))
#define _NOT_0 PROBE()

#define BOOL(x) NOT(NOT(x))

#define IF_ELSE(condition) _IF_ELSE(BOOL(condition))
#define _IF_ELSE(condition) CAT(_IF_, condition)

#define _IF_1(...) __VA_ARGS__ _IF_1_ELSE
#define _IF_0(...)             _IF_0_ELSE

#define _IF_1_ELSE(...)
#define _IF_0_ELSE(...) __VA_ARGS__

#define HAS_ARGS(...) BOOL(FIRST(_END_OF_ARGUMENTS_ __VA_ARGS__)())
#define _END_OF_ARGUMENTS_() 0

#define MAP(m, first, ...)           \
  m(first)                           \
  MAP(m, __VA_ARGS__)   \

#define _MAP() MAP

#define GREET(x) Hello, x!
EVAL(MAP(GREET, Mum, Dad, Adam, Joe))
"#, r#"Hello, Mum! MAP(GREET, Dad, Adam, Joe) a"#);
}

#[test]
fn turning_recursion_into_an_iterator_4() {
    equals_str(
        r#"
#define FIRST(a, ...) a
#define SECOND(a, b, ...) b

#define EMPTY()

#define EVAL(...) EVAL4(__VA_ARGS__)
#define EVAL1024(...) EVAL512(EVAL512(__VA_ARGS__))
#define EVAL512(...) EVAL256(EVAL256(__VA_ARGS__))
#define EVAL256(...) EVAL128(EVAL128(__VA_ARGS__))
#define EVAL128(...) EVAL64(EVAL64(__VA_ARGS__))
#define EVAL64(...) EVAL32(EVAL32(__VA_ARGS__))
#define EVAL32(...) EVAL16(EVAL16(__VA_ARGS__))
#define EVAL16(...) EVAL8(EVAL8(__VA_ARGS__))
#define EVAL8(...) EVAL4(EVAL4(__VA_ARGS__))
#define EVAL4(...) EVAL2(EVAL2(__VA_ARGS__))
#define EVAL2(...) EVAL1(EVAL1(__VA_ARGS__))
#define EVAL1(...) __VA_ARGS__

#define DEFER1(m) m EMPTY()
#define DEFER2(m) m EMPTY EMPTY()()
#define DEFER3(m) m EMPTY EMPTY EMPTY()()()
#define DEFER4(m) m EMPTY EMPTY EMPTY EMPTY()()()()

#define IS_PROBE(...) SECOND(__VA_ARGS__, 0)
#define PROBE() ~, 1

#define CAT(a,b) a ## b

#define NOT(x) IS_PROBE(CAT(_NOT_, x))
#define _NOT_0 PROBE()

#define BOOL(x) NOT(NOT(x))

#define IF_ELSE(condition) _IF_ELSE(BOOL(condition))
#define _IF_ELSE(condition) CAT(_IF_, condition)

#define _IF_1(...) __VA_ARGS__ _IF_1_ELSE
#define _IF_0(...)             _IF_0_ELSE

#define _IF_1_ELSE(...)
#define _IF_0_ELSE(...) __VA_ARGS__

#define HAS_ARGS(...) BOOL(FIRST(_END_OF_ARGUMENTS_ __VA_ARGS__)())
#define _END_OF_ARGUMENTS_() 0

#define MAP(m, first, ...)           \
  m(first)                           \
  IF_ELSE(HAS_ARGS(__VA_ARGS__))(    \
    DEFER2(_MAP)()(m, __VA_ARGS__)   \
  )(                                 \
    /* Do nothing, just terminate */ \
  )
#define _MAP() MAP

#define GREET(x) Hello, x!
EVAL(MAP(GREET, Mum, Dad, Adam, Joe))
"#, r#"Hello, Mum! Hello, Dad! Hello, Adam! Hello, Joe!"#);
}
