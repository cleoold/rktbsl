#lang racket

;; computes definite integrals here. Get rid of Wolframalpha.

;; --------------------------------------------------------------------

;; computes the Riemann sum with "infinitesimal" bars for a one-variable function y=f(x) from x=a to x=b. must note that
;; the subintervals used here do not have equal lengths. given a step (like 0.0001 units), this calculates 
;; the area of the bar from x=a to x=a+0.0001, then plus the area from x=a+0.0001 to x=a+0.0002, and so on. at the end, 
;; the second bound x=a+c*0.0001 might exceed the required x=b for some integer c. for this, an area from x=a+(c-1)*0.0001 to x=b is computed
;; to terminate the program. this can be avoided by increasing the number of partitions (decreasing step).

;; helper functions
;; area-sq: Num Num Num -> Num
;; area/acc/temp: (Num -> Num) Num Num Num Num ((list Num Num) -> Num) -> Num

(define (area-sq x-1 x-2 height) (* (- x-2 x-1) height))

(define (area/acc/temp f b step x-1 x-2 method)
  (cond
    [(>= x-2 b) (area-sq x-1 b (f (method (list x-1 b))))]
    [else (+ (area-sq x-1 (+ x-2 step) (f (method (list x-1 (+ x-2 step)))))
             (area/acc/temp f b step (+ x-1 step) (+ x-2 step) method))]))
             
;; the area derived via midpoint rule
;; area-fx-rectangular: (Num -> Num) Num Num Num -> Num
;; f is the function y=f(x), a is the starting point, b is the endpoint, step is ... the step. same for the next ones

(define (area-fx-rectangular f a b step)
  (area/acc/temp f b step a a
    (lambda (interval) (/ (+ (car interval) (cadr interval)) 2))))
   
;; the area derived via left Riemann sum
;; area-fx-rectangular: (Num -> Num) Num Num Num -> Num

(define (area-fx-rectangular-left f a b step)
  (area/acc/temp f b step a a
    (lambda (interval) (car interval))))
   
;; the area derived via right Riemann sum
;; area-fx-rectangular: (Num -> Num) Num Num Num -> Num

(define (area-fx-rectangular-right f a b step)
  (area/acc/temp f b step a a
    (lambda (interval) (cadr interval))))
   
;; example
;evaluate the area (signed) under the curve y=f(x)=x^(1/5)*e^x from x=4 to x=10, with left, midpoint and right methods
;(area-fx-rectangular-left (λ (x) (* (expt x 1/5) (exp x))) 4 10 0.001) -> 34055.70296945007
;(area-fx-rectangular (λ (x) (* (expt x 1/5) (exp x))) 4 10 0.001) -> 34073.117304433
;(area-fx-rectangular-right (λ (x) (* (expt x 1/5) (exp x))) 4 10 0.001) -> 34090.54052245134

;; midpoint method usually yields the most accurate results. however it does not work for divergent integrals. you need to
;; check its convergence by left or right methods. for example, the area for y=f(x)=sin(x)^2/x^3 is divergent from x=0 to x=10
;(area-fx-rectangular (λ (x) (/ (sqr (sin x)) (expt x 3))) 0 10 0.1) -> 4.4928857718142545
;(area-fx-rectangular (λ (x) (/ (sqr (sin x)) (expt x 3))) 0 10 0.01) -> 6.795608737685372
;(area-fx-rectangular (λ (x) (/ (sqr (sin x)) (expt x 3))) 0 10 0.00001) -> 13.703365408992292
; but...
;(area-fx-rectangular-left (λ (x) (/ (sqr (sin x)) (expt x 3))) 0 10 0.01) -> /: division by zero

;; --------------------------------------------------------------------

;; computes the length of a curve defined by y=f(x) from x=a to x=b in a similar manner as above.

;; rec-distance: (Num -> Num) Num Num -> Num
;; curve-length/acc:(Num -> Num) Num Num Num Num -> Num
;; curve-length: (Num -> Num) Num Num Num -> Num

(define (rec-distance f x1 x2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- (f x2) (f x1))))))

(define (curve-length/acc f b step x-1 x-2)
  (cond
    [(>= x-2 b) (rec-distance f x-1 b)]
    [else (+ (rec-distance f x-1 (+ x-2 step))
             (curve-length/acc f b step (+ x-1 step) (+ x-2 step)))]))

(define (curve-length f a b step)
  (curve-length/acc f b step a a))
  
;; example
;evaluate the length of the curve formed by y=f(x)=√(1-x^2) from x=-1 to x=1. produces complex error terms.
;(real-part (curve-length (λ (x) (sqrt (- 1 (sqr x)))) -1 1 0.0001) ) -> 3.1415923596240027

