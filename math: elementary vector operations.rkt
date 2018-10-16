;; calculates some basic operations associated with vectors
;; requires two vectors are same length, which means both in R^n

;; defining a vector
;; a vec is either
;; * (cons Num empty)
;; * (cons Num (cons Num empty))

;; vector addition
;; vec-add: Vec Vec -> Vec
(define (vec-add vec1 vec2)
  (cond [(empty? vec1) empty]
        [else
         (cons (+ (first vec1) (first vec2))
               (vec-add (rest vec1) (rest vec2)))]))

example
; (vec-add (list 1 2 3 4 8) (list -1 2 4 4 0)) yields
; (list 0 4 7 8 8)
