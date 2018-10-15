;; this will solve a cubic equation

;; definition of the cubic
(define-struct cubic (a b c d))
;; where a: Num, b: Num, c: Num, d: Num
;; coefficients a, b, c, d are real
;; a is not 0

;; format of a cubic equation
;; (make-cubic a b c d) corresponds to
;; ax^3+bx^2+cx+d=0
