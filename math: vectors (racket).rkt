#lang racket
;; calculates some basic operations associated with vectors
;; requires two vectors are same length, which means both in R^n

;; defining a vector
;; a Vec is either
;; * (cons Num null)
;; * (cons Num Vec)

;; ..........................................................
;; square of a vector
;; vec-sqr: Vec -> Num

(define (vec-sqr u)
  (foldr (lambda (x1 xn) (+ (sqr x1) xn)) 0 u))
  
;; ..........................................................
;; modulo of a vector
;; vec-mag: Vec -> Num

(define (vec-mag u)
  (sqrt
   (foldr (lambda (x1 xn) (+ (sqr x1) xn)) 0 u)))
   
;; ..........................................................
;; vector addition
;; vec-add: Vec Vec -> Vec

(define (vec-add u v)
  (map + u v))

;; ..........................................................
;; scalar multiplication
;; vec-mult: Num Vec -> Vec

(define (vec-mult c u)
  (foldr (lambda (x1 xn) (cons (* x1 c) xn)) '() u))
  
;; ..........................................................
;; inner product
;; vec-dot: Vec Vec -> Num

(define (vec-dot u v)
  (foldr (lambda (f1 fn) (+ f1 fn)) 0 (map * u v)))
  
;; ..........................................................
;; cross product
;; vec-cross: Vec Vec -> Vec
;; u, v length = 3

(define (vec-cross u v)
  (list (- (* (cadr u) (caddr v)) (* (caddr u) (cadr v)))
        (- (* (caddr u) (car v)) (* (car u) (caddr v)))
        (- (* (car u) (cadr v)) (* (cadr u) (car v)))))
        
;; ==========================================================
;; two vectors shall be same length

;; projection of vector x onto vector u
;; (also perpendicular of vector x onto plane with normal n)
;; proj: Vec Vec -> Vec

(define (proj u x)
  (foldr
   (lambda (x1 xn)
     (cons (* x1 (/ (foldr (lambda (f1 fn) (+ f1 fn)) 0 (map * x u))
                    (foldr (lambda (x1 xn) (+ (sqr x1) xn)) 0 u))) xn))
   '() u))
