#lang racket

;; a collection of different sorting algorithms

;; ...-sort: (listof Num) (Num Num -> Bool) -> (listof Num)

;; [insertion sort]
(define (insertion-sort lon pred?)
  (local
    [(define (insert n slon)
       (cond [(null? slon) (cons n null)]
             [(pred? n (car slon)) (cons n slon)]
             [else (cons (car slon) (insert n (cdr slon)))]))]
    (cond [(null? lon) null]
          [else (insert (car lon) (insertion-sort (cdr lon) pred?))])))
          
          
;; [merge sort]          
(define (merge-sort lon pred?)
  (local
    [(define (merge-lists l1 l2)
       (cond
         [(null? l1) l2]
         [(null? l2) l1]
         [(pred? (car l1) (car l2))
          (cons (car l1) (merge-lists (cdr l1) l2))]
         [else
          (cons (car l2) (merge-lists l1 (cdr l2)))]))]
    (cond
      [(or (null? lon) (null? (cdr lon))) lon]
      [(null? (cddr lon))
       (merge-lists (list (car lon)) (cdr lon))]
      [else
       (let ([x (ceiling (/ (length lon) 2))])
         (merge-lists (merge-sort (take lon x) pred?)
                      (merge-sort (drop lon x) pred?)))])))
                      
                      
;; [quick sort]    
(define (quick-sort lon pred?)
  (cond
    [(null? lon) null]
    [else
     (local
       [(define pivot (car lon))
        (define left
          (filter (lambda (x) (pred? x pivot)) (cdr lon)))
        (define right
          (filter (lambda (x) ((compose not pred?) x pivot)) (cdr lon)))]
       (append (quick-sort left pred?) (list pivot) (quick-sort right pred?)))]))
