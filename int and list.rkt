#lang racket

;; conversions between integers and (listof Int)s!

(provide integer->intlist intlist->integer)

;; definitions
;; an integer (Int) is an Int
;; a positive list of integers is (listof Nat)
;; a negative list of integers is (cons '- (listof Nat))

;; converts an integer to a list of integers
;; integer->intlist: Int -> LoI

(define (integer->intlist my-int)
  (if (>= my-int 0)
      (map (lambda (x) (- (char->integer x) (char->integer #\0)))
           ((compose string->list number->string) my-int))
      (cons '- (integer->intlist (- my-int)))))

;; examples
;(integer->intlist 1234) -> '(1 2 3 4)
;(integer->intlist -1234) -> '(- 1 2 3 4)

;; converts a list of integers to an integer 
;; integer->intlist: LoI -> Int

(define (intlist->integer my-lst)
  (if (integer? (car my-lst))
      ((compose string->number list->string)
       (map (lambda (x) (integer->char (+ 48 x))) my-lst))
      (- (intlist->integer (cdr my-lst)))))
      
;; examples
;(intlist->integer '(1 2 3 4)) -> 1234
;(intlist->integer '(- 1 2 3 4)) -> -1234
