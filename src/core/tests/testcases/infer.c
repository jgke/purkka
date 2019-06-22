int _lambda_1(int left, int right) {
    int res = left + right;
    return res;
}
void _lambda_2(void (*f)(int ), int *list, int n) { {
        for(int i = 0; i < n;(i) ++) {
            f(list[i]);
        }
    }
}
int foo() {
    int a = 1;
    int b = a;
    return b;
}
int (*bar)(int left, int right) = _lambda_1;
void (*for_each)(void (*f)(int ), int *list, int n) = _lambda_2;
