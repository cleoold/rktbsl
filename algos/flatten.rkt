#lang racket

;; flatten: Nest-List-Num -> (listof Num)

(define (flatten lst)
  (cond [(empty? lst) empty]
        [(number? (car lst))
         (cons (car lst) (flatten (cdr lst)))]
        [else (append (flatten (car lst))
                      (flatten (cdr lst)))]))
                      
#| nested list template
(define (nest-lst-template lst)
  (cond [(empty? lst) ... ]
        [(number? (car lst))
         (... (car lst) ... (nest-lst-template (cdr lst)) ... )]
        [else
         (... (nest-lst-template (car lst)) ...
              (nest-lst-template (cdr lst)) ... )])) |#;
