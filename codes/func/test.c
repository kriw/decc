int add(int a, int b) {
    return a + b;
}
int main() {
    int a = 0;
    int b = 2;
    int c = add(a, b);
    add(b, c);
    return 0;
}
