#include "dependencies/macro.h"

struct foo {
    int a;
};
int main() {
    return(1 + 1);
}
int qux() {
    return((1 + 1));
}
int baz() {
    {
        int FOO = 1;
        (void)FOO;
    }
    return((1 + 1));
}
int bar() {
    return 0;
}