#lang racket

(provide newton-solve)

;; computations for recurrence sequences ready for use

;; with one offset and one term for recursion eg:
;; a(0) = A
;; a(n) = f(n-1)

;; recursive-sequence1: X (X -> X) -> (Nat -> X)

(define (recursive-sequence1 A f)
  (lambda (n)
    (cond
      [(= n 0) A]
      [else
       (local
         [(define (adder a counter)
            (cond [(= n counter) (f a)]
                  [else (adder (f a) (add1 counter))]))]
         (adder A 1))])))
         
;; example
;{ a(0)=1, a(n)=2*a(n-1) }  first 10 terms
;(build-list 10 (recursive-sequence1 1 (λ (x) (* 2 x)))) -> '(1 2 4 8 16 32 64 128 256 512)


;; with two offsets and two terms for recursion eg:
;; a(0) = A
;; a(1) = B
;; a(n) = f(n-2, n-1)

;; recursive-sequence2: X X (X X -> X) -> (Nat -> X)

(define (recursive-sequence2 A B f)
  (lambda (n)
    (cond
      [(= n 0) A]
      [(= n 1) B]
      [else
       (local
         [(define (adder a b counter)
            (cond [(= n counter) (f a b)]
                  [else (adder b (f a b) (add1 counter))]))]
         (adder A B 2))])))
         
;; example
;{ a(0)=1, a(1)=1 a(n)=a(n-2)+a(n-1)^2 } first 7 terms
;(build-list 7 (recursive-sequence2 1 1 (λ (x y) (+ x (sqr y))))) -> '(1 1 2 5 27 734 538783)

;; ................................................................................
;; solve equations f=0 with Newton's method
;; f is the function that equals 0, guess is the initial guess, step is the step used in computing derivatives
;; newton-solve: (Num -> Num) Num Num -> (Nat -> Num)
;; requires: step is greater than 0

(require "math-gradient.rkt")

(define (newton-solve f guess step)
  (recursive-sequence1 guess
    (lambda (x) (- x (/ (f x) (right-derivative f x step))))))

;; example
;solve cos(x)-x=0
;((newton-solve (λ (x) (- (cos x) x)) -4 0.000001) 10) -> 0.7390851332151607
