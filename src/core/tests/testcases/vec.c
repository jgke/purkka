#include "dependencies/vec.h"

int foo() {
    vec_int a = (vec_int) {
         1,
         2
    };
    int b = a[0];
    return b;
}
