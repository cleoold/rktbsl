#lang racket

;; approximation of rate of change of functions

;; definition
;; a point or vector is a list of numbers


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


;; directional derivative of a surface defined in R^n with equation f along a specific directional vector l[cosα, cosβ, ..., cosN]
;; f is the function, vars is the list of variable names, x0 is the point of interest, 
;; direction is the list of degrees for each direction (in radians), step is ... the step

(define (directional-derivative f vars x0 direction step)
  (foldr (lambda (vars_1 di_1 vd_n)
           (+ (* (p-derivative f vars x0 vars_1 step) (cos di_1)) vd_n))
         0 vars direction))
         
;; example
;∂/∂l [f(x,y,z)=xy+yz+zx] @(x,y,z)=(1,1,2), where l has angles [60°, 45°, 60°]
;(directional-derivative (λ (x y z) (+ (* x y) (* y z) (* z x))) '(x y z) '(1 1 2)
;                        `(,(degrees->radians 60) ,(degrees->radians 45) ,(degrees->radians 60)) 0.0001) -> 4.621320343559594


;; gradient (as a vector) of a surface defined in R^n with equation f
;; (also the modulo of maximum directional derivative)
;; f is the function, vars is the list of variable names, x0 is the point of interest, 
;; step is ... the step

(define (gradient f vars x0 step)
  (foldr (lambda (vars_1 vars_n)
           (cons (p-derivative f vars x0 vars_1 step) vars_n))
         null vars))
         
;; example
;∇ [f(x,y,z)=x+2y+cosz] @(x,y,z)=(411,7,-1)
;(gradient (λ (x y z) (+ x (* 2 y) (cos z))) '(x y z) '(411 7 -1) 0.00001) ->
; '(0.9999999974752426 2.0000000006348273 0.8414682838520092)
