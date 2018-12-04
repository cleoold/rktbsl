# lang racket

;; things about Fibonacci #'s

;; regular version

(define (fibonacci i)
  (cond
    [(= i 0) 0]
    [(= i 1) 1]
    [else
     (+ (fibonacci (- i 1)) (fibonacci (- i 2)))]))
     
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
    
 ;; tail recursion v3
  
 (define (fibonacci4 i)
  (cond
    [(= i 0) 0]
    [(= i 1) 1]
    [else
     (local
       [(define (fib-adder a b counter)
       (cond [(= i counter) (+ a b)]
             [else (fib-adder b (+ a b) (add1 counter))]))]
       (fib-adder 0 1 2))]))
