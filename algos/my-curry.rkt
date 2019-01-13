#lang racket

;; notes about defining some higher order functions

;; my-curry: (X Y -> A) -> (X -> Y -> A)
(define (my-curry f)
 (lambda (x)
    (lambda (y)
     (f x y))))
     
;; my-uncurry: (X -> Y -> A) -> (X Y -> A)
(define (my-uncurry f)
  (lambda (x y) ((f x) y)))
  
;; my-compose: (Y -> A) (X -> Y) -> (X -> A)
(define (my-compose f g)
  (lambda (x) (f (g x))))
 
;; filter-yes:
(define (keep-apples lst)
  (filter ((my-curry symbol=?) 'apple) lst))

;; filter-not:
(define (eat-apples lst)
  (filter (my-compose not ((my-curry symbol=?) 'apple)) lst))
  
;; my-map: (X -> Y) (listof X) -> (listof Y)
(define (my-map f lst)
  (foldr (lambda (x y) (cons (f x) y)) empty lst))
  
(define (my-map2 f lst)
  (foldr (my-uncurry (my-compose (my-curry cons) f)) empty lst))
