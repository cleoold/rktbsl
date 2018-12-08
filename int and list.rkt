#lang racket

;; conversions between integers and (listof Int)s!

(provide integer->intlist 
         intlist->integer 
         charlist->hexintlist 
         hexintlist->charlist)

;; definitions
;; an integer (Int) is an Int
;; a positive list of integers is (listof Nat)
;; a negative list of integers is (cons '- (listof Nat))

;; converts an integer to a list of integers
;; integer->intlist: Int -> LoI

(define (integer->intlist my-int)
  (if (>= my-int 0)
      (map (lambda (x) (- (char->integer x) 48)) ; (char->integer #\0) -> 48
           ((compose string->list number->string) my-int))
      (cons '- (integer->intlist (- my-int)))))

;; examples
;(integer->intlist 1234) -> '(1 2 3 4)
;(integer->intlist -1234) -> '(- 1 2 3 4)

;; =========================================================================

;; converts a list of integers to an integer 
;; integer->intlist: LoI -> Int

;; [extracting by char]
(define (intlist->integer my-lst)
  (if (integer? (car my-lst))
      ((compose string->number list->string)
       (map (lambda (x) (integer->char (+ 48 x))) my-lst))
      (- (intlist->integer (cdr my-lst)))))
      
;; [extracting by decimal expansion]
(define (intlist->integer2 my-lst)
  (if (integer? (car my-lst))
      (foldl (lambda (x acc) (+ x (* acc 10))) 0 my-lst)
      (- (intlist->integer2 (cdr my-lst)))))
      
;; examples
;(intlist->integer '(1 2 3 4)) -> 1234
;(intlist->integer '(- 1 2 3 4)) -> -1234

;; =========================================================================

;; converts a list of chars to a list of hex integers
;; charlist->hexintlist: (listof Char) -> (listof (anyof Int Sym))

(define (charlist->hexintlist my-lst)
  (foldr (lambda (i1 rr)
           (cond [(char-alphabetic? i1)
                  (cons (string->symbol (make-string 1 i1)) rr)]
                 [(equal? i1 #\-) (cons '- rr)]
                 [else (cons (- (char->integer i1) (char->integer #\0)) rr)]))
         null my-lst))
         
;; example
;(charlist->hexintlist '(#\- #\1 #\2 #\3 #\A #\F)) -> '(- 1 2 3 A F)

;; =========================================================================

;; converts a list of hex integers to a list of chars
;; hexintlist->charlist: (listof (anyof Int Sym)) -> (listof Char)

(define (hexintlist->charlist my-lst)
  (foldr (lambda (i1 rr)
           (if (integer? i1)
               (cons (integer->char (+ 48 i1)) rr)
               (cons ((compose car string->list symbol->string) i1) rr)))
         null my-lst))
      
;; example
;(hexintlist->charlist '(- 1 2 3 A F)) -> '(#\- #\1 #\2 #\3 #\A #\F)
