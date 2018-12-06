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
     (define degree (length numlst))
     (define (if-positive degreel numlst)
       (foldr (lambda (numl1 degl1 rr)
                (+ (* numl1 (expt nbase degl1)) rr))
              0
              numlst
              (reverse (build-list degree identity))))]
    (if (>= nnumber 0)
        (if-positive degree numlst)
        (- (if-positive degree numlst)))))
           
;; [faster expansion]
(define (basen->dec nbase nnumber)
  (if (zero? nnumber)
      nnumber
      (+ (modulo nnumber 10) 
         (* nbase (basen->dec nbase (quotient nnumber 10))))))
         
         
;; converts a decimal integer to a base-n integer
;; basen->dec: Nat Int -> Int

(define (dec->basen nbase nnumber)
  (local
    [(define (dec-basen/reversed nnumber)
       (if (zero? nnumber)
           null
           (cons (modulo nnumber nbase)
                 (dec-basen/reversed (quotient nnumber nbase)))))]
    (reverse (dec-basen/reversed nnumber))))
    
;; ======================================================================
