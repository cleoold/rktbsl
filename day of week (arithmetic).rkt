;; given a date after Gregorian calendar being effective 
;; returns the day of the week
;; input format: yyyymmdd

;; date->day-of-week: Nat -> Sym

(define (date->day-of-week date)
  (cond [(= (w date) 0) 'Sunday]
        [(= (w date) 1) 'Monday]
        [(= (w date) 2) 'Tuesday]
        [(= (w date) 3) 'Wednesday]
        [(= (w date) 4) 'Thursday]
        [(= (w date) 5) 'Friday]
        [(= (w date) 6) 'Saturday]))
        
(date->day-of-week 20180926) "shall be" 'Wednesday

;; .........................................................
;; parameters

;; shifted month m

(define (m date)
  (cond [(> (- (quotient date 100) (* (quotient date 10000) 100)) 2)
         (- (quotient date 100) (* (quotient date 10000) 100) 2)]
         [else
          (- (quotient date 100) (* (quotient date 10000) 100) -10)]))
          
;; day of month d

(define (d date)
  (- date (* (quotient date 100) 100)))
  
;; shifted year yyyy

(define (yyyy date)
  (cond [(>= (m date) 11)
         (- (quotient date 10000) 1)]
        [else
         (quotient date 10000)]))
         
;; last two digits of yyyy y

(define (y date)
  (- (yyyy date) (* (quotient (yyyy date) 100) 100)))
  
;; first two digits of yyyy c

(define (c date)
  (quotient (yyyy date) 100))
  
;; day of week w

(define (w date)
  (modulo (+ (d date) (floor (- (* 2.6 (m date)) 0.2))
           (y date) (floor (/ (y date) 4))
           (floor (/ (c date) 4)) (* -2 (c date))) 7))
