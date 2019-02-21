#include <stdio.h>
#include "math_recurrence_sequence.h"
#include "math_derivatives.h"

// computations for recurrence sequences ready for use

// with one offset and one term for recursion ie:
// a(0) = A
// a(n) = f(n-1)

// recursive_sequence1: returns the n-th term of the sequence {a=f(n)}, indexed from 0.
double recursive_sequence1(double (*f)(double), const double A, const int n) {
  int i;
  double curr = A;
  for (i = n; i > 0; --i) {
    curr = f(curr);
  }
  return curr;
}

// build_recursive_sequence1: prints a list of terms from n=0 to n.
void build_recursive_sequence1(double (*f)(double), const double A, const int n) {
  int i;
  double curr = A;
  printf("%lf, ", A);
  for (i = n; i > 0; --i) {
    curr = f(curr);
    printf("%lf, ", curr);
  }
  printf("\n");
}

/* example
 { a(0)=1, a(n)=2*a(n-1) }  first 10 terms:

     double f(double x) { return x * 2; }
     ...
     build_recursive_sequence1(f, 1, 9);
     >>
     1.000000, 2.000000, 4.000000, 8.000000, 16.000000, 32.000000, 64.000000, 128.000000, 256.000000, 512.000000, 
*/


// with two offsets and two terms for recursion ie:
// a(0) = A
// a(1) = B
// a(n) = f(n-2, n-1)

// recursive-sequence2: returns the n-th term of the sequence {a=f(n)}, indexed from 0.
double recursive_sequence2(double (*f)(double, double), const double A, const double B, const int n) {
  if (n == 0) return A;
  if (n == 1) return B;

  int i;
  double curr1 = A;
  double curr2 = B;
  double new;

  for (i = n; i > 1; --i) {
    new = f(curr1, curr2);
    curr1 = curr2;
    curr2 = new;
  }
  return curr2;
}

// build_recursive_sequence1: prints a list of terms from n=0 to n.
void build_recursive_sequence2(double (*f)(double, double), const double A, const double B, const int n) {
  int i;
  double curr1 = A;
  double curr2 = B;
  double new;
  printf("%lf, ", A);
  if (n >= 1) printf("%lf, ", B);

  for (i = n; i > 1; --i) {
    new = f(curr1, curr2);
    curr1 = curr2;
    curr2 = new;
    printf("%lf, ", curr2);
  }
  printf("\n");
}

/* example
 { a(0)=1, a(1)=1 a(n)=a(n-2)+a(n-1)^2 } first 7 terms:

    double f(double x, double y) { return x + pow(y, 2); }
    ...
     build_recursive_sequence2(f, 1, 1, 6);
     >>
     1.000000, 1.000000, 2.000000, 5.000000, 27.000000, 734.000000, 538783.000000, 
*/


// ................................................................................
// solve equations f=0 with Newton's method
// f is the function that equals 0, guess is the initial guess, step is the step used in computing derivatives
// requires: step is greater than 0

double newton_solve(double (*f)(double), const double x0, const double step, const int n) {
  int i;
  double curr = x0;
  for (i = n; i > 0; --i) {
    curr = curr - f(curr) / right_derivative(f, curr, step);
  }
  return curr;
}

// newtonsol: pre-configured with step length 0.000001 and recurring times 50.
double newton_sol(double (*f)(double), const double x0) {
  return newton_solve(f, x0, 0.000001, 50);
}

/* example
 solve cos(x)-x=0

     double f(double x) { return cos(x)-x; }
     ...
     newton_solve(f, -4, 0.000001, 10) >> 0.73985
*/

