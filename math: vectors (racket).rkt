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
