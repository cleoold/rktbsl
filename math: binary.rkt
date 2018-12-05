#lang racket

;; something about base-n conversion

;; converts a base-n integer to a decimal integer
;; basen->dec: Num Nat -> Num
;; requires: nbase > 0

;; [exponential expansion]
(define (basen->dec2 nbase nnumber)
  (local
    [(define numlst (integer->list nnumber))
     (define degree (length numlst))]
    (foldr (lambda (numl1 degl1 rr)
             (+ (* numl1 (expt nbase degl1)) rr))
           0
           numlst
           (reverse (build-list degree identity)))))
           
;; [faster expansion]
(define (basen->dec nbase nnumber)
  (if (zero? nnumber)
      nnumber
      (+ (modulo nnumber 10) 
         (* nbase (basen->dec nbase (quotient nnumber 10))))))
