#ifndef math_derivatives_h
#define math_derivatives_h

// right derivative of function f(x)
double right_derivative(double (*f)(double), double x0, double step);
// left derivative of function f(x)
double left_derivative(double (*f)(double), double x0, double step);


#endif