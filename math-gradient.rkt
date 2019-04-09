#lang racket

(provide right-derivative)

;; approximation of rate of change of functions

;; definition
;; a point or vector is a list of numbers

;; notice on second derivatives:
;;    the function approximates the 'rate of change' of rate of change: ie it computes two first derivates,
;;    then gets the difference and divides. as such the precision decreases dramatically if our step is too small.


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


;; this function uses the above functions to calculate the rate of change of rate of change
;; f is the function, x0 is the point of interest, step is ... the step
;; the step (h) should be not too small
;; lr-second-derivative: (Num -> Num) Num Num -> Num

(define (lr-second-derivative f x0 step)
  (define l (left-derivative f x0 step))
  (define r (right-derivative f (+ x0 step) step))
  (/ (- r l) (* 2 step)))

;; example
;(d/dx^2 [f(x)=sqrt(x^3+sin(x))] @x=2
;(lr-second-derivative (λ (x) (sqrt (+ (expt x 3) (sin x)))) 3.152 0.00001) -> 0.5066702613021334
;; note the amount of digits in the argument 'step'
; now assume D- = D+




;; this is a helper function that selects the corresponding component of a vector given its name, and modifies it.
;; modify-variables: (listof Sym) (listof Num) Sym (Num -> Num) -> (listof Num)
;; requires: vars and x0 have same lengths

(define (modify-variable vars x0 my-var op)
  (foldr
   (lambda (x0_1 x_1 rr)
     (cond [(symbol=? x_1 my-var) (cons (op x0_1) rr)]
           [else (cons x0_1 rr)]))
   null x0 vars))


;; partial rate of change with respect to one variable, of a function that maps a subset of R to a subset of R
;; f is the function, vars is the list of variables names, x0 is the point of interest, 
;; my-var is the variable with respect to, step is ... the step
;; p-derivative: (Num ... Num -> Num) (listof Sym) (listof Num) Sym Num -> Num
;; requires: vars and x0 have same lengths

(define (p-derivative f vars x0 my-var step)
  (/ (- (apply f (modify-variable vars x0 my-var (lambda (x) (+ x step))))
        (apply f x0))
     step))
     
;; example
;∂/∂y [f(x,y,z)=x+2y+cosz] @(x,y,z)=(411,7,-1)
;(p-derivative (λ (x y z) (+ x (* 2 y) (cos z))) '(x y z) '(411 7 -1) 'y 0.00001) -> 2.0000000006348273


;; directional derivative of a surface defined in R^n with equation f along a specific directional vector l[cosα, cosβ, ..., cosN]
;; f is the function, vars is the list of variable names, x0 is the point of interest, 
;; direction is the list of degrees for each direction (in radians), step is ... the step
;; directional-derivative: (Num ... Num -> Num) (listof Sym) (listof Num) (listof Num) Num -> Num
;; requires: vars and x0 have same lengths

(define (directional-derivative f vars x0 direction step)
  (foldr (lambda (vars_1 di_1 rr)
           (+ (* (p-derivative f vars x0 vars_1 step) (cos di_1)) rr))
         0 vars direction))
         
;; example
;∂/∂l [f(x,y,z)=xy+yz+zx] @(x,y,z)=(1,1,2), where l has angles [60°, 45°, 60°]
;(directional-derivative (λ (x y z) (+ (* x y) (* y z) (* z x))) '(x y z) '(1 1 2)
;                        `(,(degrees->radians 60) ,(degrees->radians 45) ,(degrees->radians 60)) 0.0001) -> 4.621320343559594


;; gradient (as a vector) of a surface defined in R^n with equation f
;; (also the modulo of maximum directional derivative)
;; f is the function, vars is the list of variable names, x0 is the point of interest, 
;; step is ... the step
;; gradient: (Num ... Num -> Num) (listof Sym) (listof Num) Num -> (listof Num)
;; requires: vars and x0 have same lengths

(define (gradient f vars x0 step)
  (foldr (lambda (vars_1 rr)
           (cons (p-derivative f vars x0 vars_1 step) rr))
         null vars))
         
;; example
;∇ [f(x,y,z)=x+2y+cosz] @(x,y,z)=(411,7,-1)
;(gradient (λ (x y z) (+ x (* 2 y) (cos z))) '(x y z) '(411 7 -1) 0.00001) ->
; '(0.9999999974752426 2.0000000006348273 0.8414682838520092)


;; this function is like p-derivative, but it again computes the secondary rate of change with respect to a variable
;; f is the function, vars is the list of variables names, x0 is the point of interest, 
;; my-var is the variable with respect to, step is ... the step
;; lr-second-p-derivative: (Num ... Num -> Num) (listof Sym) (listof Num) Sym Num -> Num
;; requires: vars and x0 have same lengths

(define (lr-second-p-derivative f vars x0 my-var step)
  (define l (/ (- (apply f x0)
                  (apply f (modify-variable vars x0 my-var (lambda (x) (- x step))))) step))
  (define r (/ (- (apply f (modify-variable vars x0 my-var (lambda (x) (+ x step))))
                  (apply f x0)) step))
  (/ (- r l) step))

;; example
;(∂/∂y)^2 [f(x,y,z,t)=sqrt(x)+(x)sqrt(y)+sqrt(z)] @(x,y,z,t)=(5,2,1,999)
;(lr-second-p-derivative (λ(x y z t) (+ (sqrt x) (* x (sqrt y)) (sqrt z))) '(x y z t) '(5 2 1 999) 'y 0.00001) -> -0.44193981807438826
