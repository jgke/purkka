#include "dependencies/macro.h"

int main() {
    return(1 + 1);
}
int bar() {
    return((1 + 1));
}
int baz() {
    {
        int FOO = 1;
        (void)FOO;
    }
    return((1 + 1));
}
