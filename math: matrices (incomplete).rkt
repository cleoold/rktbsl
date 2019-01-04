;#lang racket
;; some operations on matrices
;; a matrix is really a series of rows/columns of vectors so
;; refer to "rktbsl/math: vectors.rkt" for required functions

(require "math: vectors (racket).rkt")

;; defining a matrix
;; a matrix (Mtx) is a list
;; (cons (list of Num) null)
;; (cons (list of Num) Mtx)

;; ..........................................................
;; addition of two matrices
;; two Mtx are same size
;; vec-add: (listof Num) (listof Num) -> (listof Num)
;; mtx-add: Mtx Mtx -> Mtx

; requires (vec-add u v)

(define (mtx-add a b)
  (cond [(null? a) null]
        [else 
         (cons (vec-add (car a) (car b))
               (mtx-add (cdr a) (cdr b)))]))
                    
;; example
; (add-mtx
;    '((1 2 3 4) (2 4 6 5) (-8 6 4 0))
;    '((6 9 4 4) (-9 -7 -5 3) (7 4 -3 0)))
; yields
; '((7 11 7 8) (-7 -3 1 8) (-1 10 1 0))

;; ..........................................................
;; scalar multiplication 
;; vec-mult: Num (listof Num) -> (listof Num)
;; mtx-sc-mult: Num Mtx -> Mtx

; requires (vec-mult c u)

(define (mtx-sc-mult c a)
  (cond [(null? a) null]
        [else 
         (cons (vec-mult c (car a))
               (mtx-sc-mult c (cdr a)))]))
               
;; example
; (mtx-sc-mult 2
; '((1 2 3 4) (-2 -3 -4 -5)))
; yields
; '((2 4 6 8) (-4 -6 -8 -10))

;; ..........................................................
;; transpose
;; transpose: Mtx -> Mtx

(define (transpose2 m)
  (local
    [(define (get-ele row jth)
       (if (zero? jth)
           (car row)
           (get-ele (cdr row) (sub1 jth))))
     (define (column colth m2)
       (if (null? m2)
           null
           (cons (get-ele (car m2) colth) (column colth (cdr m2)))))
     (define (trp/raw from)
       (if (= from (length (car m)))
           null
           (cons (column from m) (trp/raw (add1 from)))))]
    (trp/raw 0)))
    
;; [with alf]
(define (transpose m)
  (apply map list m))
    
;; example
; (transpose 
; '((1 2 3 4) (5 6 7 8)))
; yields
; '((1 5) (2 6) (3 7) (4 8))


