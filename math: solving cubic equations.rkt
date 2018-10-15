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

(define (soln-cubic mycubic)
  (make-soln
   (- (t 0 mycubic) (/ (cubic-b mycubic) (* 3 (cubic-a mycubic))))
   (- (t 1 mycubic) (/ (cubic-b mycubic) (* 3 (cubic-a mycubic))))
   (- (t 2 mycubic) (/ (cubic-b mycubic) (* 3 (cubic-a mycubic))))))
             
;; example
; (soln-cubic (make-cubic 1 -4 1 6)) yields
; (make-soln 3.0 2.0000000000000004 -1.0000000000000002)

;; ==================================================================

;; algebraic solving method (copied from Wikipedia too)
;; (line 6) coefficients a, b, c, d can be C
;; (solnx-cubic mycubic): Cubic -> Soln

(define (d mycubic)
  (+ (* 18 (cubic-a mycubic) (cubic-b mycubic) (cubic-c mycubic) (cubic-d mycubic))
     (* -4 (expt (cubic-b mycubic) 3) (cubic-d mycubic))
     (* (sqr (cubic-b mycubic)) (sqr (cubic-c mycubic)))
     (* -4 (cubic-a mycubic) (expt (cubic-c mycubic) 3))
     (* -27 (sqr (cubic-a mycubic)) (sqr (cubic-d mycubic)))))

(define (d0 mycubic)
  (- (sqr (cubic-b mycubic)) (* 3 (cubic-a mycubic) (cubic-c mycubic))))

(define (d1 mycubic)
  (+ (* 2 (expt (cubic-b mycubic) 3))
     (* -9 (cubic-a mycubic) (cubic-b mycubic) (cubic-c mycubic))
     (* 27 (sqr (cubic-a mycubic)) (cubic-d mycubic))))

(define (c mycubic)
  (cond
    [(and (= (d0 mycubic) 0) (= (+ (d1 mycubic) (sqr (d1 mycubic))) 0))
     (expt
      (/ (- (d1 mycubic) (sqrt (- (sqr (d1 mycubic)) (* 4 (expt (d0 mycubic) 3)))))
         2) 1/3)]
    [else
     (expt
      (/ (+ (d1 mycubic) (sqrt (- (sqr (d1 mycubic)) (* 4 (expt (d0 mycubic) 3)))))
         2) 1/3)]))

(define (solnx-cubic mycubic)
  (make-soln
   (* (/ -1 (* 3 (cubic-a mycubic)))
      (+ (cubic-b mycubic) (c mycubic) (/ (d0 mycubic) (c mycubic))))
   (* (/ -1 (* 3 (cubic-a mycubic)))
      (+ (cubic-b mycubic) (* (+ -1/2 (* 1/2 (sqrt 3) 0+i)) (c mycubic))
         (/ (d0 mycubic) (* (+ -1/2 (* 1/2 (sqrt 3) 0+i)) (c mycubic)))))
   (* (/ -1 (* 3 (cubic-a mycubic)))
      (+ (cubic-b mycubic) (* (- -1/2 (* 1/2 (sqrt 3) 0+i)) (c mycubic))
         (/ (d0 mycubic) (* (- -1/2 (* 1/2 (sqrt 3) 0+i)) (c mycubic)))))))
         
;; examples
   
; (solnx-cubic (make-cubic 1 2 3 4)) yields
; (make-soln
;  -1.6506291914393878
;  -0.17468540428030593-1.546868887231396i
;  -0.17468540428030593+1.546868887231396i)

; (solnx-cubic (make-cubic 0+2i -2+i 0 4)) yields
; (make-soln
;  0.9239464076086398+0.3893781800865609i
;  -0.274545429916065-1.6650945552905423i
;  -1.1494009776925749+0.27571637520398135i)
