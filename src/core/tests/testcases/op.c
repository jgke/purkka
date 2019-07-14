struct Tuple;
struct Tuple {
    int left;
    int right;
};
inline struct Tuple _lambda_0(struct Tuple left, struct Tuple right) {
    return (struct Tuple) {
        .left = left.left + right.left,
        .right = left.right + right.right
    }
    ;
}
int main() {
    struct Tuple left = (struct Tuple) {
        .left = 0,
        .right = 0
    };
    struct Tuple right = (struct Tuple) {
        .left = 1,
        .right = 1
    };
    struct Tuple mid = _lambda_0(left, right);
    return mid.left;
}
