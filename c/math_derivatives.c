#include <stdio.h>
#include "math_derivatives.h"

// approximation of rate of change of functions


// rate of change of a single-variable function
// f is the function, x0 is the point of interest, step is ... the step

// [right derivative]
double right_derivative(double (*f)(double), double x0, double step) {
    return (f(x0 + step) - f(x0)) / step;
}

// [left derivative]
double left_derivative(double (*f)(double), double x0, double step) {
    return (f(x0) - f(x0 - step)) / step;
}

/* example
  d/dx [f(x)=(1/x)sin(x^2)] @x=2

    double f(double x) { return sin(pow(x, 2)) / x; }
    ...
    right_derivative(1, 2, 0.00001) >> -1.118054

 now assume D- = D+
*/