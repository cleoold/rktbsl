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
;; parameters and conversions

;; date converted to string datestr

(define (datestr date)
  (number->string date))
  
;; shifted month m

(define (m date)
  (cond [(> (string->number (substring (datestr date) 4 6)) 2)
         (- (string->number (substring (datestr date) 4 6)) 2)]
        [else
         (+ (string->number (substring (datestr date) 4 6)) 10)]))
         
;; day of month d

(define (d date)
  (string->number (substring (datestr date) 6)))
  
;; shifted year yyyy

(define (yyyy date)
  (number->string (cond [(>= (m date) 11)
         (- (string->number (substring (datestr date) 0 4)) 1)]
        [else
         (string->number (substring (datestr date) 0 4))])))
         
;; last two digits of yyyy y

(check-expect (y 17530101) 52)

(define (y date)
  (string->number (substring (yyyy date) 2)))
  
;; first two digits of yyyy c

(define (c date)
  (string->number (substring (yyyy date) 0 2)))
  
;; day of week w

(define (w date)
  (modulo (+ (d date) (floor (- (* 2.6 (m date)) 0.2))
           (y date) (floor (/ (y date) 4))
           (floor (/ (c date) 4)) (* -2 (c date))) 7))
