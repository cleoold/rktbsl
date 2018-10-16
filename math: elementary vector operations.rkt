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

;; example
; (vec-add (list 1 2 3 4 8) (list -1 2 4 4 0)) yields
; (list 0 4 7 8 8)


;; scalar multiplication
;; vec-mult: Num Vec -> Vec

(define (vec-mult c vec1)
  (cond [(empty? vec1) empty]
        [else
         (cons (* c (first vec1))
               (vec-mult c (rest vec1)))]))
               
;; example
; (vec-mult -2 (list 2 -3 4 5)) yields
; (list -4 6 -8 -10)


;; inner product
;; vec-dot: Vec Vec -> Num

(define (vec-dot vec1 vec2)
  (cond [(empty? vec1) 0]
        [else
         (+ (* (first vec1) (first vec2))
         (vec-dot (rest vec1) (rest vec2)))]))
         
;; example
; (vec-dot (list 1 2 3 4) (list 2 3 4 5)) yields
; 40

;; cross product
;; vec-cross: Vec Vec -> Vec
;; vec1, vec2 length = 3

(define (vec-cross vec1 vec2)
  (list (- (* (second vec1) (third vec2)) (* (third vec1) (second vec2)))
        (- (* (third vec1) (first vec2)) (* (first vec1) (third vec2)))
        (- (* (first vec1) (second vec2)) (* (second vec1) (first vec2)))))
        
;; example
; (vec-cross (list 1 2.3 -0.2) (list 2.2 3 1.5)) yields
; (list 4.05 -1.94 -2.06)
