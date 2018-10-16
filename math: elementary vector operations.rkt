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
