int f(int a) {
    return a;
}
void cast() {
    int (*a)(int a) = &f;
    void * (*b)(int ) = (void *(*)(int ))a;
    (void)b;
}
