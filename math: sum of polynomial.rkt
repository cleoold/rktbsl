;; computes the result of a given polymonial using Horner's method

;; a Poly (polynomial) is one of
;; * (cons Num empty)
;; * (cons Num poly)

;; format of the polynomial
;; (list a-0 a-1 a-2 ... a-n) corresponds to 
;; (a-0)+(a-1)x^1+(a-2)x^2+...+(a-n)x^n

;; eval-poly: Poly -> Num

(define (eval-poly mypoly x)
  (cond [(empty? (rest mypoly)) (first mypoly)]
        [else 
         (+ (first mypoly) (* x (eval-poly (rest mypoly) x)))]))
         
; example
; 1+2x+3x^2-4x^3-5x^4 = -479 (x=3)
(eval-poly (list 1 2 3 -4 -5) 3)
"shall be"
-479
