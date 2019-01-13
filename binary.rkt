#lang racket

;; something about base-n conversion and applications in binary conversion


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


(require "algos/int-and-list.rkt")

;; converts a hexadecimal integer to a decimal integer
;; hex->dec Str -> Nat

(define (hex->dec an-int)
  (local
    [(define hexintlst (string->list an-int))
     (define (intlst hexlst)
       (foldr (lambda (hexl1 rr)
                (cond [(char=? hexl1 #\A) (cons 10 rr)] [(char=? hexl1 #\B) (cons 11 rr)]
                      [(char=? hexl1 #\C) (cons 12 rr)] [(char=? hexl1 #\D) (cons 13 rr)]
                      [(char=? hexl1 #\E) (cons 14 rr)] [(char=? hexl1 #\F) (cons 15 rr)]
                      [else (cons (- (char->integer hexl1) (char->integer #\0)) rr)]))
              null hexintlst))]
    (foldr (lambda (numl1 degl1 rr)
             (+ (* numl1 (expt 16 degl1)) rr))
           0
           (intlst an-int)
           (reverse (build-list (length hexintlst) identity)))))
           
;; example
;(hex->dec "1597FA") -> 1415162


;; converts a decimal integer to a hexadecimal integer
;; dec->hex: Nat -> Str

(define (dec->hex an-int)
  ((compose list->string hexintlist->charlist)
   (foldr (lambda (i1 rr)
            (cond [(= i1 10) (cons 'A rr)] [(= i1 11) (cons 'B rr)]
                  [(= i1 12) (cons 'C rr)] [(= i1 13) (cons 'D rr)]
                  [(= i1 14) (cons 'E rr)] [(= i1 15) (cons 'F rr)]
                  [else (cons i1 rr)]))
          null (dec->basen 16 an-int))))
          
;; example
;(dec->hex 1415162) -> "1597FA"
