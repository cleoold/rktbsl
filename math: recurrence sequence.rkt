#lang racket
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
;{ a(0)=1, a(n)=2*(n-1) }  first 10 terms
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
;{ a(0)=1, a(1)=1 a(n)=(n-2)+(n-1)^2 } first 7 terms
;(build-list 7 (recursive-sequence2 1 1 (λ (x y) (+ x (sqr y))))) -> '(1 1 2 5 27 734 538783)
