;; a correct answer counts 2 points, an incorrect answer counts 1 point,
;; 0 point counted for blank answers.
;; to calculate the final grade, best 75% are selected
;;
;; this calculates the grade

(define (mygrade total correct incorrect)
  (* 100 (/ (- (+ (* 3/4 total) (min (* 3/4 total) correct))
     (max 0 (- (* 3/4 total) correct incorrect))) (* 3/4 total 2))))
    
;; example
; (mygrade 125 50 50) "shall be" 230/3
