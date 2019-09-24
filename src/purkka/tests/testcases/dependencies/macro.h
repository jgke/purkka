#define FOO(a) (a + 1)

#define BAR(a) int bar() { return 0; }

#define BAZ() struct foo { int a; };

#ifdef CONDITIONAL
#define CONDITIONAL_VALUE CONDITIONAL
#else
#error "Define the CONDITIONAL macro"
#endif
