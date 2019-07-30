void for_loop() {
    for(int i = 0; i < 5; i++) {
    }
}
void if_stmts() {
    char *foo;
    if(1) {
        foo = "bar";
    }
    int bar;
    if(1) {
        bar = 1;
    }
    else {
        bar = 2;
    }
    (void)foo;
    (void)bar;
}
void blocks_0() {
    int a = 1;
    (void)a;
}
void blocks_1() {
    {
        int a = 1;
        (void)a;
    }
}
void blocks_2() {
    {
        {
            int a = 1;
            (void)a;
        }
    }
}
