#ifndef math_num_integral_h
#define math_num_integral_h

// mid_riemann: the area derived via midpoint rule
double mid_riemann(double (*f)(double x), double a, double b, double step);
// left_riemann: the area derived via left Riemann sum method
double left_riemann(double (*f)(double x), double a, double b, double step);
// right_riemann: the area derived via right Riemann sum method
double right_riemann(double (*f)(double x), double a, double b, double step);
// curve_length: computes the length of a curve.
double curve_length(double (*f)(double x), double a, double b, double step);

#endif