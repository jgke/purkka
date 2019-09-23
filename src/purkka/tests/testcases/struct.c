struct Foo;
struct Bar;
struct Foo {
    int bar;
    int baz;
};
struct Bar {
    struct Foo foo;
};
int main() {
    struct Foo a = {.bar = 0,.baz = 1, };
    struct Foo *b = &a;
    struct Foo **c = &b;
    return a.bar +(*b).baz +(**c).baz;
}
int ternary() {
    struct Foo a = {.bar = 0,.baz = 0, };
    struct Foo *b = &a;
    return(*b).bar ?(*b).bar :(*b).bar;
}
void nested() {
    struct Bar a = {.foo = {.bar = 2,.baz = 1, }, };
    (void)a;
}
