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
       
;; ..................................................................

;; with offsets and weights
;; F(0) = A
;; F(1) = B
;; F(n) = xF(n-2) + yF(n-1)

(define (newfib i A B x y)
  (cond
    [(= i 0) A]
    [(= i 1) B]
    [else
     (local
       [(define (fibh a b counter)
       (cond [(= i counter) (+ (* 2 x) (* 3 y))]
             [else (fibh b (+ (* 2 x) (* 3 y)) (add1 counter))]))]
       (fibh A B 2))]))
