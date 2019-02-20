// finds the n-th root of a real (double) number, ie solving the equation y=f(x)=x^n-a=0 for x.
// with neccessary functions not pre-defined.

#include <stdio.h>

const double precision = 0.00001;

// power(x, y) returns x raised by integer power y. y is positive.
double power(const double x, int y) {
  if (y < 0) {
	  printf("error: y is not negative.");
	  return 0;
  }
  double result = 1;
  for (; y; --y) {
    result *= x;
  }
  return result;
}


// abs_value(x) returns the absolute value of x.
double abs_value(const double x) {
  return (x > 0) ? x : -x;
}


// nth_root(a, n) algorithm returns nth root of number a. n is positive, and that negative a doesn't have even real roots.
double nth_root(const double a, const int n) {
  if (n <= 0 || (a < 0 && n % 2 == 0)) {
	  printf("error: n > 0, and negative numbers don't have even real roots.");
	  return 0;
  }
  double x_k = 1;
  while (abs_value(power(x_k, n) - a) >= precision) {
  x_k = ((n - 1) * x_k + a / power(x_k, n - 1)) / n;
  }
  return x_k;
}


int main (void) {
	//printf("%f\n", nth_root(2.525,3));
}
