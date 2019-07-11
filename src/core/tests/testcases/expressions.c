int main() {
    int a;
    a = 1 + 2;
    a = 0 ? 1 : 2;
    a = 1 ? 1 : 2;
    int *b = &a;
    int *c = b + 1 - 1;
    int *d = b - 1 + 1;
    int e = *b;
    return a + *b + *c + *d + e;
}
