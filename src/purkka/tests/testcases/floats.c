double floats() {
    float a = 1.0;
    float b = 1.0;
    float c = a + b;
    double d = 2.0;
    double e = 2.0;
    double f = d + e;
    float g = 3.0;
    double h = 3.0;
    double i = (double)g + h;
    double j = 3.0;
    float k = 3.0;
    float l = (float)j + k;
    return (double)c + f + i + (double)l;
}
