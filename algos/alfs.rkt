#lang racket
;; implements some useful list-modification functions


;; snaps a slice of a list from index-from, to just before index-to
;; subsequence: (listof Any) Nat Nat -> (listof Any)
;; requires: to >= from

(define (subsequence lst from to)
  (foldr (lambda (l1 j rr)
           (if (and (>= j from) (< j to))
               (cons l1 rr)
               rr))
         null lst (build-list (length lst) identity)))

;; example
;(subsequence '(0 1 2 3 4 5 6) 2 4) -> '(2 3)


;; keeps only the first occurence of equal elements in the list
;; dedup: (listof X) -> (listof X)

(define (dedup lon)
  (foldr (lambda (n0 rr)
           (cons n0 (filter (lambda (n1) (not (eq? n1 n0))) rr)))
           empty lon))
  
;; example
;(dedup '(1 2 2 3 4 4 3 2 1)) -> '(1 2 3 4)
