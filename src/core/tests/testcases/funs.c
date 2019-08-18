void recursive() {
    recursive();
}
void arr_arg(char *s) {
    (void)s;
}
void arr_call() {
    char *s = "foo";
    arr_arg(s);
}
