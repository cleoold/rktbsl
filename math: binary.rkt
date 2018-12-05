#lang racket

;; something about base-n conversion

;; converts a base-n number to a decimal number
;; basen->dec: Num Nat -> Num
;; requires: nbase > 0

(define (basen->dec nbase nnumber)
  (local
    [(define numlst
       (map (lambda (x) (- (char->integer x) (char->integer #\0)))
            ((compose string->list number->string) nnumber)))
     (define degree (length numlst))]
    (foldr (lambda (numl1 degl1 rr)
             (+ (* numl1 (expt nbase degl1)) rr))
           0
           numlst
           (reverse (build-list degree identity)))))
 
;; v2
(define (basen->dec2 nbase nnumber)
  (if (zero? nnumber)
      nnumber
      (+ (modulo nnumber 10) 
         (* nbase (basen->dec2 nbase (quotient nnumber 10))))))
