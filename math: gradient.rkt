#lang racket

;; approximation of rate of change of functions


;; rate of change of a single-variable function
;; f is the function, x0 is the point of interest, step is ... the step
;; smaller step (h) doesn't guarantee smaller error, but probably makes more
;; -derivative: (Num -> Num) Num Num -> Num

;; [right derivative]
(define (right-derivative f x0 step)
  (/ (- (f (+ x0 step)) (f x0)) step))

;; [left derivative]
(define (left-derivative f x0 step)
  (/ (- (f x0) (f (- x0 step))) step))
  
;; example
;d/dx [f(x)=(1/x)sin(x^2)] @x=2
;(right-derivative (λ (x) (* (/ x) (sin (sqr x)))) 2 0.000000001) -> -1.1180867343085765
; now assume D- = D+


;; partial rate of change with respect to one variable, of a function that maps a subset of R to a subset of R
;; f is the function, vars is the list of variables names, x0 is the point of interest, 
;; my-var is the variable with respect to, step is ... the step
;; p-derivative: (Num ... Num -> Num) (listof Sym) (listof Num) Sym Num -> Num
;; requires: vars and x0 have same lengths

(define (p-derivative f vars x0 my-var step)
  (/ (- (apply f (foldr
                  (lambda (x0_1 x_1 x0_n)
                    (cond [(symbol=? x_1 my-var) (cons (+ x0_1 step) x0_n)]
                          [else (cons x0_1 x0_n)]))
                  null x0 vars))
        (apply f x0))
     step))
     
;; example
;∂/∂y [f(x,y,z)=x+2y+cosz] @(x,y,z)=(411,7,-1)
;(p-derivative (λ (x y z) (+ x (* 2 y) (cos z))) '(x y z) '(411 7 -1) 'y 0.00001) -> 2.0000000006348273
