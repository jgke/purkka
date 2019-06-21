int _lambda_1(int left, int right) {
    int res = left + right;
    return res;
}
int foo() {
    int a = 1;
    int b = a;
    return b;
}
int (*bar)(int left, int right) = _lambda_1;
