;; computes the product of two quaternions

;; format of quaternion
(define-struct quaternion (cc ic jc kc))
;; A Quaternion is a (make-quaternion Num Num Num Num)
;; data example
(make-quaternion 1 2 3 4)

;; quat-mult: quaternion quaternion -> quaternion

(define (quat-mult a b q1 q2)
  (make-quaternion
   (+ (*(quaternion-cc q1) (quaternion-cc q2))
      (*(quaternion-ic q1) (quaternion-ic q2) a)
      (*(quaternion-jc q1) (quaternion-jc q2) b)
      (*(quaternion-kc q1) (quaternion-kc q2) -1 a b))
   (+ (*(quaternion-cc q1) (quaternion-ic q2))
      (*(quaternion-ic q1) (quaternion-cc q2))
      (*(quaternion-jc q1) (quaternion-kc q2) -1 b)
      (*(quaternion-kc q1) (quaternion-jc q2) b))
   (+ (*(quaternion-cc q1) (quaternion-jc q2))
      (*(quaternion-ic q1) (quaternion-kc q2) a)
      (*(quaternion-jc q1) (quaternion-cc q2))
      (*(quaternion-kc q1) (quaternion-ic q2) -1 a))
   (+ (*(quaternion-cc q1) (quaternion-kc q2))
      (*(quaternion-ic q1) (quaternion-jc q2))
      (*(quaternion-jc q1) (quaternion-ic q2) -1)
      (*(quaternion-kc q1) (quaternion-cc q2)))))
      
; example
; (6+0i-2j-1k)*(1+0i+4j+2k) = 114+0i+22j+11k (a=10, b=9)

(quat-mult 10 9 (make-quaternion 6 0 -2 -1) (make-quaternion 1 0 4 2))
"shall be"
(make-quaternion 114 0 22 11)