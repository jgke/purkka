struct Foo;
struct Foo {
    int bar;
    int baz;
};
int main() {
    struct Foo a = (struct Foo) {
        .bar = 0,
        .baz = 1
    };
    struct Foo *b = &a;
    struct Foo **c = &b;
    return a.bar +(*b).baz +(**c).baz;
}
