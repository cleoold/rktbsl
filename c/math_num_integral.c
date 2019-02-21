#include <stdio.h>
#include <math.h>
#include "math_num_integral.h"

// computes the Riemann sum with "infinitesimal" bars for a one-variable function y=f(x) from x=a to x=b. must note that
// the subintervals used here do not have equal lengths. given a step (like 0.0001 units), this calculates 
// the area of the bar from x=a to x=a+0.0001, then plus the area from x=a+0.0001 to x=a+0.0002, and so on. at the end, 
// the second bound x=a+c*0.0001 might exceed the required x=b for some integer c. for this, an area from x=a+(c-1)*0.0001 
// to x=b is computed to terminate the program. this can be avoided by increasing the number of partitions (decreasing step).

// mid_riemann: the area derived via midpoint rule
// f is the function y=f(x), a is the starting point, b is the endpoint, step is ... the step. same for the next ones
double mid_riemann(double (*f)(double x), const double a, const double b, double step) {
    double curr;
    double x_1 = a;
    double x_2 = a + step;
    while (x_2 < b) {
        curr += (x_2 - x_1) * (f(x_1) + f(x_2)) / (double)2;
        x_1 = x_2;
        x_2 += step;
    }
    curr += (b - x_1) * (f(x_1) + f(b)) / (double)2;
    return curr;
}

// left_riemann: the area derived via left Riemann sum method
double left_riemann(double (*f)(double x), const double a, const double b, double step) {
    double curr;
    double x_1 = a;
    double x_2 = a + step;
    while (x_2 < b) {
        curr += (x_2 - x_1) * f(x_1);
        x_1 = x_2;
        x_2 += step;
    }
    curr += (b - x_1) * f(x_1);
    return curr;
}

// right_riemann: the area derived via right Riemann sum method
double right_riemann(double (*f)(double x), const double a, const double b, double step) {
    double curr;
    double x_1 = a;
    double x_2 = a + step;
    while (x_2 < b) {
        curr += (x_2 - x_1) * f(x_2);
        x_1 = x_2;
        x_2 += step;
    }
    curr += (b - x_1) * f(b);
    return curr;
}

/* example
 evaluate the area (signed) under the curve y=f(x)=x^(1/5)*e^x from x=4 to x=10, with left, midpoint and right methods

    double f(double x) {
    return pow(x, (double)1/(double)5) * exp(x);
    }
    ...
    left_riemann(f, 4, 10, 0.001) >> 34055.702969
    mid_riemann(f, 4, 10, 0.001) >> 34073.121746
    right_riemann(f, 4, 10, 0.001) >> 34090.540522

 this does not work for divergent integral functions. in which case all three would yield inconsistent results, and in
 particular left and right sums could encounter division by 0.
*/


// curve_length: computes the length of a curve defined by y=f(x) from x=a to x=b in a similar manner as above.
double curve_length(double (*f)(double x), const double a, const double b, double step) {
    double curr;
    double x_1 = a;
    double x_2 = a + step;
    while (x_2 < b) {
        curr += sqrt(pow(x_2 - x_1, 2) + pow(f(x_2) - f(x_1), 2));
        x_1 = x_2;
        x_2 += step;
    }
    curr += sqrt(pow(b - x_1, 2) + pow(f(b) - f(x_1), 2));
    return curr;
}

/* example
 evaluate the length of the curve formed by y=f(x)=√(1-x^2) from x=-1 to x=1.

    double f(double x) {
    return sqrt(1 - pow(x, 2));
    }
    ...
    curve_length(f, -1, 1, 0.0001) >> 3.141592
*/
