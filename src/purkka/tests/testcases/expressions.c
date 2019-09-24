static int A = 1;
static int B = 2;
static int C = 3;
int main() {
    int a;
    a = 1 + 2;
    a = 0 ? 1 : 2;
    a = 1 ? 1 : 2;
    int *b = &a;
    int *c = b + 1 - 1;
    int *d = b - 1 + 1;
    int e = *b;
    int f[3] = { 1, 2, 3, };
    (void)f;
    int g[] = { 1, 2, 3, };
    (void)g;
    double h = 1.23e+12;
    double i = 1e-12;
    (void)h;
    (void)i;
    (void)sizeof(char *[5]);
    (void)sizeof(char[3]);
    return a + *b + *c + *d + e;
}
