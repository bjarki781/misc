#include<stdio.h>
#include<math.h>

double f(double x);
double D(double (*func)(double), double x);

int main()
{
    double x = M_PI_4;
    printf("f: sin, f': cos, x=PI/4, f(x) = %f, f'(x) =%f\n", f(x), D(f, x));
}

double f(double x)
{
    return sin(x);
}

double D(double (*func)(double), double x)
{
    double dx = (nextafter(nextafter(x, x+1), x+1) - nextafter(x, x+1));
    return ((*func)(x+dx)-(*func)(x)) / dx;
}
