;; calculates some basic operations associated with vectors
;; requires two vectors are same length, which means both in R^n

;; defining a vector
;; a vec is either
;; * (cons Num empty)
;; * (cons Num (cons Num empty))

;; ..........................................................
;; modulo of a vector
;; vec-sqr: Vec -> Num ; this determines the square of vector u
;; vec-mag: Vec -> Num

(define (vec-sqr u)
  (cond [(empty? u) 0]
        [else
         (+ (sqr (first u)) (vec-sqr (rest u)))]))
(define (vec-mag u)
  (sqrt (vec-sqr u)))
 
;; example
; (vec-mag (list -2 1 3)) yields
; 3.7416573867739413

;; ..........................................................
;; vector addition
;; vec-add: Vec Vec -> Vec

(define (vec-add u v)
  (cond [(empty? u) empty]
        [else
         (cons (+ (first u) (first v))
               (vec-add (rest u) (rest v)))]))

;; example
; (vec-add (list 1 2 3 4 8) (list -1 2 4 4 0)) yields
; (list 0 4 7 8 8)

;; ..........................................................
;; scalar multiplication
;; vec-mult: Num Vec -> Vec

(define (vec-mult c u)
  (cond [(empty? u) empty]
        [else
         (cons (* c (first u))
               (vec-mult c (rest u)))]))
               
;; example
; (vec-mult -2 (list 2 -3 4 5)) yields
; (list -4 6 -8 -10)

;; ..........................................................
;; inner product
;; vec-dot: Vec Vec -> Num

(define (vec-dot u v)
  (cond [(empty? u) 0]
        [else
         (+ (* (first u) (first v))
         (vec-dot (rest u) (rest v)))]))
         
;; example
; (vec-dot (list 1 2 3 4) (list 2 3 4 5)) yields
; 40

;; ..........................................................
;; cross product
;; vec-cross: Vec Vec -> Vec
;; u, v length = 3

(define (vec-cross u v)
  (list (- (* (second u) (third v)) (* (third u) (second v)))
        (- (* (third u) (first v)) (* (first u) (third v)))
        (- (* (first u) (second v)) (* (second u) (first v)))))
        
;; example
; (vec-cross (list 1 2.3 -0.2) (list 2.2 3 1.5)) yields
; (list 4.05 -1.94 -2.06)

;; ==========================================================
;; below contents need above functions ready as helpers
;; two vectors shall be same length

;; projection of vector x onto vector u
;; (also perpendicular of vector x onto plane with normal n)
;; proj: Vec Vec -> Vec

(define (proj u x)
  (vec-mult
   (/ (vec-dot x u) (vec-sqr u)) u))
   
;; example
; (proj (list 4 0 3) (list 1 -5 3)) yields
; (list 2.08 0 1.56)

;; ..........................................................
;; perpendicular of vector x onto vector u
;; (also projection of vector x onto plane with normal n)
;; perp: Vec Vec -> Vec

(define (perp u x)
  (vec-add x (vec-mult -1 (vec-mult (/ (vec-dot x u) (vec-sqr u)) u))))
  
;; example
; (perp (list 2 -1 3 2) (list 1 2 -3 4)) yields
; (list 1.1- 1.94- 2.83- 4.1-)

;; ..........................................................
;; reflection of vector x onto hyperplane with normal n
;; refl: Vec Vec -> Vec

(define (refl n x)
  (vec-add x (vec-mult -2 (vec-mult (/ (vec-dot x n) (vec-sqr n)) n))))
  
;; example
; (refl (list 1 -2 3) (list 1 0 0)) yields
; (list 6/7 2/7 -3/7)

;; ..........................................................
;; rotates 2d vector v counterclockwise with angle a
;; vector v shall have length 2
;; r2: Vec Num -> Vec

(define (r2 v a)
  (list
   (- (* (cos a) (first v)) (* (sin a) (second v)))
   (+ (* (sin a) (first v)) (* (cos a) (second v)))))

;; example
; (r2 (list 1 0) (/ pi 2)) yields
; (list -1.0 6.123031769111886e-017)

;; ..........................................................
;; rotate 3d vector v counterclockwise with angle a with respect with x, y, or z axis
;; vector shall have length 3
;; rx, ry, rz: Vec Num -> Vec

(define (rx v a)
  (list
   (first v)
   (- (* (second v) (cos a)) (* (third v) (sin a)))
   (+ (* (second v) (sin a)) (* (third v) (cos a)))))

(define (ry v a)
  (list
   (+ (* (first v) (cos a)) (* (third v) (sin a)))
   (second v)
   (- (* (third v) (cos a)) (* (first v) (sin a)))))

(define (rz v a)
  (list
   (- (* (first v) (cos a)) (* (second v) (sin a)))
   (+ (* (first v) (sin a)) (* (second v) (cos a)))
   (third v)))

