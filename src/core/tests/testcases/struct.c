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
    return a.bar + a.baz;
}
