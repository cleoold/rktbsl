#lang racket
;; calculates some basic operations associated with vectors
;; requires two vectors are same length, which means both in R^n

(provide vec-add vec-mult vec-dot)

;; defining a vector
;; a Vec is either
;; * (cons Num null)
;; * (cons Num Vec)

;; ..........................................................
;; square of a vector
;; vec-sqr: Vec -> Num

(define (vec-sqr u)
; alt:  (foldr (lambda (x1 xn) (+ (sqr x1) xn)) 0 u))
  (apply + (map sqr u)))
  
;; ..........................................................
;; modulo of a vector
;; vec-mag: Vec -> Num

(define (vec-mag u)
  (sqrt
   (apply + (map sqr u))))
   
;; ..........................................................
;; vector addition
;; vec-add: Vec Vec -> Vec

(define (vec-add u v)
  (map + u v))

;; ..........................................................
;; scalar multiplication
;; vec-mult: Num Vec -> Vec

(define (vec-mult c u)
  (map (lambda (u1) (* c u1)) u))
  
;; ..........................................................
;; inner product
;; vec-dot: Vec Vec -> Num

(define (vec-dot u v)
  (foldr + 0 (map * u v)))
  
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
  (local [(define (vec-mult c u)
            (map (lambda (u1) (* c u1)) u))
          (define (vec-dot u v)
            (foldr + 0 (map * u v)))
          (define (vec-sqr u)
            (vec-dot u u))]
    (vec-mult (/ (vec-dot x u) (vec-sqr u)) u)))
