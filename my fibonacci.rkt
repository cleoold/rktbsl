# lang racket

;; things about Fibonacci #'s

;; regular version

;; F(0) = 0
;; F(1) = 1
;; F(n) = F(n-2) + F(n-1)

(define (fibonacci n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else
     (+ (fibonacci (- n 1)) (fibonacci (- n 2)))]))
     
 ;; tail recursion v1
 
 (define (fibonacci2 n)
  (local
    [(define (fib-adder a b n)
       (cond [(= 0 n) (+ a b)]
             [else (fib-adder b (+ a b) (sub1 n))]))]
    (fib-adder -1 1 n)))
    
 ;; tail recursion v2
    
 (define (fibonacci3 n)
  (local
    [(define (fib-adder a b counter)
       (cond [(= n counter) (+ a b)]
             [else (fib-adder b (+ a b) (add1 counter))]))]
    (fib-adder -1 1 0)))
    
 ;; recursion v3
  
 (define (fibonacci4 n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else
     (local
       [(define (fib-adder a b counter)
       (cond [(= n counter) (+ a b)]
             [else (fib-adder b (+ a b) (add1 counter))]))]
       (fib-adder 0 1 2))]))
       
; (build-list 20 fibonacci4) -> '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181)
       
;; ..................................................................

;; with offsets and weights
;; F(0) = A
;; F(1) = B
;; F(n) = xF(n-2) + yF(n-1)

(define (weighted-fibonacci n A B x y)
  (cond
    [(= n 0) A]
    [(= n 1) B]
    [else
     (local
       [(define (fib-adder a b counter)
       (cond [(= n counter) (+ (* x a) (* y b))]
             [else (fib-adder b (+ (* x a) (* y b)) (add1 counter))]))]
       (fib-adder A B 2))]))
       
; (build-list 20
;   (lambda (n) (weighted-fibonacci n 4 6 2 -1)))
; -> '(4 6 2 10 -6 26 -38 90 -166 346 -678 1370 -2726 5466 -10918 21850 -43686 87386 -174758 349530)
       
;; ..................................................................

;; subsequence: (listof Any) Nat Nat -> (listof Any)

(define (subsequence lst from to)
  (foldr (lambda (l1 j rr)
           (if (and (>= j from) (< j to))
               (cons l1 rr)
               rr))
         null lst (build-list (length lst) identity)))
         
; (subsequence '(0 1 2 3 4 5 6) 2 4) -> '(2 3)
