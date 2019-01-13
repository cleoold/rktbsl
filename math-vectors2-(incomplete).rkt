#lang racket
;; more analyses towards vectors in linear algebra; the extension of rktbsl/math: vectors (racket)

(require "math-vectors-(racket).rkt")

;; ..........................................................
;; checks whether a vector is a zero vector
;; zero-vec?: Vec -> Bool

(define (zero-vec? u)
; alt: (foldr (lambda (u1 rr) (cond [(= u1 0) (and rr)] [else #f])) #t u))
  (= (apply + (map abs u)) 0))
  
;; example
;(zero-vec '(1 2 0)) -> #f
;(zero-vec '(0 0 0 0)) -> #t

;; ..........................................................
;; checks whether one vector is a scalar multiple of the other given two
;; is-multiple?: Vec Vec -> Bool

(define (is-multiple? u v)
  (cond
    [(or (zero-vec? u) (zero-vec? v)) #t] ; a zero vector is multiple of any
    [else
     (local
       [(define (c u v) ; determines one "common" ratio
          (cond
            [(null? u) 0]
            [(not (zero? (car v))) (/ (car u) (car v))]
            [else (c (cdr u) (cdr v))]))
        (define (obey-ratio? u v coe) ; does all components have the same ratio?
          (cond
            [(null? u) #t]
            [(and (zero? (car u)) (not (zero? (car v)))) #f]
            [(and (zero? (car v)) (not (zero? (car u)))) #f]
            [(and (zero? (car u)) (zero? (car v)))
             (obey-ratio? (cdr u) (cdr v) coe)]
            [(= (/ (car u) (car v)) coe)
             (and (obey-ratio? (cdr u) (cdr v) coe))]
            [else #f]))]
       (obey-ratio? u v (c u v)))]))
       
;; example
;(is-multiple? '(1 0 3) '(0 1 0)) -> #f
;(is-multiple? '(1 0 3) '(-2 0 -6)) -> #t
;(is-multiple? '(1 0 3) '(0 0 0)) -> #t

;; ..........................................................
