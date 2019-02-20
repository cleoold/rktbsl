#ifndef math_recurrence_sequence_h
#define math_recurrence_sequence_h

// recursive_sequence1: returns the n-th term of the sequence {a=f(n)}, indexed from 0.
double recursive_sequence1(double (*f)(double), double A, int n);
// build_recursive_sequence1: prints a list of terms from n=0 to n.
void build_recursive_sequence1(double (*f)(double), double A, int n);

// recursive-sequence2: returns the n-th term of the sequence {a=f(n)}, indexed from 0.
double recursive_sequence2(double (*f)(double, double), double A, double B, int n);
// build_recursive_sequence1: prints a list of terms from n=0 to n.
void build_recursive_sequence2(double (*f)(double, double), double A, double B, int n);

// newton_solve: solve equations f=0 with Newton's method.
double newton_solve(double (*f)(double), double x0, double step, int n);
// newton_sol: solve equations f=0 with Newton's method with pre-configured settings.
double newton_sol(double (*f)(double), double x0);


#endif