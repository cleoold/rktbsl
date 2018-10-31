;; some operations on matrices
;; a matrix is really a series of rows/columns of vectors so
;; refer to "rktbsl/math: vectors.rkt" for required functions

;; defining a matrix
;; a matrix (Mtx) is a list
;; (cons (list of Num) empty)
;; (cons (list of Num) Mtx)

;; ..........................................................
;; addition of two matrices
;; two Mtx are same size
;; vec-add: (listof Num) (listof Num) -> (listof Num)
;; mtx-add: Mtx Mtx -> Mtx

; requires (vec-add u v)

(define (mtx-add a b)
  (cond [(empty? a) empty]
        [else 
         (cons (vec-add (first a) (first b))
               (mtx-add (rest a) (rest b)))]))
                    
;; example
; (add-mtx
;    '((1 2 3 4) (2 4 6 5) (-8 6 4 0))
;    '((6 9 4 4) (-9 -7 -5 3) (7 4 -3 0)))
; yields
; '((7 11 7 8) (-7 -3 1 8) (-1 10 1 0))

;; ..........................................................
;; scalar multiplication 
;; vec-mult: Num (listof Num) -> (listof Num)
;; mtx-sc-mult: Num Mtx -> Mtx

; requires (vec-mult c u)

(define (mtx-sc-mult c a)
  (cond [(empty? a) empty]
        [else 
         (cons (vec-mult c (first a))
               (mtx-sc-mult c (rest a)))]))
               
;; example
(mtx-sc-mult 2
   '((1 2 3 4) (-2 -3 -4 -5)))
yields
'((2 4 6 8) (-4 -6 -8 -10))
