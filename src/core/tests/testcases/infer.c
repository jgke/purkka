int foo() {
    int a = 1;
    int b = a;
    return b;
}
int bar(int left, int right) {
    int res = left + right;
    return res;
}
void for_each(void (*f)(int ), int *list, int n) {
    for(int i = 0; i < n; i++) {
        f(list[i]);
    }
}
