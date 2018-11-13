;#lang racket
;; calculates some basic operations associated with vectors
;; requires two vectors are same length, which means both in R^n

;; defining a vector
;; a Vec is either
;; * (cons Num null)
;; * (cons Num Vec)

;; ..........................................................
;; modulo of a vector
;; vec-sqr: Vec -> Num ; this determines the square of vector u
;; vec-mag: Vec -> Num

(define (vec-sqr u)
  (cond [(null? u) 0]
        [else
         (+ (sqr (car u)) (vec-sqr (cdr u)))]))
(define (vec-mag u)
  (sqrt (vec-sqr u)))
 
;; example
; (vec-mag (list -2 1 3)) yields
; 3.7416573867739413

;; ..........................................................
;; vector addition
;; vec-add: Vec Vec -> Vec

(define (vec-add u v)
  (cond [(null? u) null]
        [else
         (cons (+ (car u) (car v))
               (vec-add (cdr u) (cdr v)))]))

;; example
; (vec-add (list 1 2 3 4 8) (list -1 2 4 4 0)) yields
; (list 0 4 7 8 8)

;; ..........................................................
;; scalar multiplication
;; vec-mult: Num Vec -> Vec

(define (vec-mult c u)
  (cond [(null? u) null]
        [else
         (cons (* c (car u))
               (vec-mult c (cdr u)))]))
               
;; example
; (vec-mult -2 (list 2 -3 4 5)) yields
; (list -4 6 -8 -10)

;; ..........................................................
;; inner product
;; vec-dot: Vec Vec -> Num

(define (vec-dot u v)
  (cond [(null? u) 0]
        [else
         (+ (* (car u) (car v))
         (vec-dot (cdr u) (cdr v)))]))
         
;; example
; (vec-dot (list 1 2 3 4) (list 2 3 4 5)) yields
; 40

;; ..........................................................
;; cross product
;; vec-cross: Vec Vec -> Vec
;; u, v length = 3

(define (vec-cross u v)
  (list (- (* (cadr u) (caddr v)) (* (caddr u) (cadr v)))
        (- (* (caddr u) (car v)) (* (car u) (caddr v)))
        (- (* (car u) (cadr v)) (* (cadr u) (car v)))))
        
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
   (- (* (cos a) (car v)) (* (sin a) (cadr v)))
   (+ (* (sin a) (car v)) (* (cos a) (cadr v)))))

;; example
; (r2 (list 1 0) (/ pi 2)) yields
; (list -1.0 6.123031769111886e-017)

;; ..........................................................
;; rotate 3d vector v counterclockwise with angle a with respect with x, y, or z axis
;; vector shall have length 3
;; rx, ry, rz: Vec Num -> Vec

(define (rx v a)
  (list
   (car v)
   (- (* (cadr v) (cos a)) (* (caddr v) (sin a)))
   (+ (* (cadr v) (sin a)) (* (caddr v) (cos a)))))

(define (ry v a)
  (list
   (+ (* (car v) (cos a)) (* (caddr v) (sin a)))
   (cadr v)
   (- (* (caddr v) (cos a)) (* (car v) (sin a)))))

(define (rz v a)
  (list
   (- (* (car v) (cos a)) (* (cadr v) (sin a)))
   (+ (* (car v) (sin a)) (* (cadr v) (cos a)))
   (caddr v)))
