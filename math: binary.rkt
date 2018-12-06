#lang racket

;; something about base-n conversion and applications in binary conversion

(require "int and list.rkt")

;; converts a base-n integer to a decimal "integer" (there are probably fraction base)
;; basen->dec: Num Int -> Num
;; requires: nbase > 0

;; [regular exponential expansion]
(define (basen->dec2 nbase nnumber)
  (local
    [(define numlst (integer->intlist (abs nnumber)))
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
         
         
;; converts a decimal integer to a base-n integer
;; basen->dec: Nat Nat -> (listof Nat)

(define (dec->basen nbase nnumber)
  (local
    [(define (dec-basen/reversed nnumber)
       (if (zero? nnumber)
           null
           (cons (modulo nnumber nbase)
                 (dec-basen/reversed (quotient nnumber nbase)))))]
    (reverse (dec-basen/reversed nnumber))))
    
;; ======================================================================

;; converts a binary integer to a decimal integer
;; bin->dec: Nat -> Nat

(define (bin->dec an-int)
  (((curry basen->dec) 2) an-int))
  
;; example
;(dec->bin 1234) -> 10011010010


;; converts a decimal integer to a binary integer
;; dec->bin: Nat -> Nat

(define (dec->bin an-int)
  (intlist->integer 
    (((curry dec->basen) 2) an-int)))
    
;; example
;(bin->dec 10011010010) -> 1234
