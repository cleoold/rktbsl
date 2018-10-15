;; this will solve a cubic equation

;; definition of the cubic
(define-struct cubic (a b c d))
;; where a: Num, b: Num, c: Num, d: Num
;; coefficients a, b, c, d are real
;; a is not 0

;; its solution soln is
(define-struct soln (x1 x2 x3))
;; where x1, x2, x3 are Num

;; format of a cubic equation
;; (make-cubic a b c d) corresponds to
;; ax^3+bx^2+cx+d=0

;; trignometric solving method (copied from Wikipedia)
;; this is applicable for three real roots (p != 0)
;; (soln-cubic mycubic): Cubic -> Soln

(define (soln-cubic mycubic)
  (make-soln
   (- (t 0 mycubic) (/ (cubic-b mycubic) (* 3 (cubic-a mycubic))))
   (- (t 1 mycubic) (/ (cubic-b mycubic) (* 3 (cubic-a mycubic))))
   (- (t 2 mycubic) (/ (cubic-b mycubic) (* 3 (cubic-a mycubic))))))

(define (p mycubic)
  (/ (- (* 3 (cubic-a mycubic) (cubic-c mycubic)) (sqr (cubic-b mycubic)))
     (* 3 (sqr (cubic-a mycubic)))))

(define (q mycubic)
  (/ (+ (* 2 (expt (cubic-b mycubic) 3))
        (* -9 (cubic-a mycubic) (cubic-b mycubic) (cubic-c mycubic))
        (* 27 (sqr (cubic-a mycubic)) (cubic-d mycubic)))
     (* 27 (expt (cubic-a mycubic) 3))))

(define (t k mycubic)
  (* 2 (sqrt (* -1/3 (p mycubic)))
     (cos (- (* 1/3 (acos (* 3/2 (/ (q mycubic) (p mycubic)) (sqrt (/ -3 (p mycubic))))))
             (* 2/3 pi k)))))
             
;; example
; (soln-cubic (make-cubic 1 -4 1 6)) yields
; (make-soln 3.0 2.0000000000000004 -1.0000000000000002)

;; ==================================================================

;; algebraic solving method (copied from Wikipedia too)
;; (line 6) coefficients a, b, c, d are in C
;; (solnx-cubic mycubic): Cubic -> Soln
