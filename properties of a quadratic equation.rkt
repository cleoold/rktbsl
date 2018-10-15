;; computes the two roots of a quadratic equation

;; definition of the quadratic
(define-struct quad (a b c))
;; where a: Num, b: Num, c: Num

;; its solution Soln is
(define-struct soln (x1 x2))
;; where x1: Num, x2: Num

;; format of the quadratic equation
;; (make-quad a b c) corresponds to
;; ax^2+bx+c=0

;; soln-quad: Quad -> Soln

(define (soln-quad myquad)
  (make-soln
   (/ (+ (- (quad-b myquad)) (d myquad)) (* 2 (quad-a myquad)))
   (/ (+ (- (quad-b myquad)) (- (d myquad))) (* 2 (quad-a myquad)))))

(define (d myquad)
  (sqrt (- (sqr (quad-b myquad)) (* 4 (quad-a myquad) (quad-c myquad)))))

;; examples

;(soln-quad (make-quad 2 3 2/3)) yields
;(make-soln -0.27128644612183095 -1.228713553878169)

;(soln-quad (make-quad 1 4 4)) yields
;(make-soln -2 -2)

;(soln-quad (make-quad 1 0 4)) yields
;(make-soln 0+2i 0-2i)
