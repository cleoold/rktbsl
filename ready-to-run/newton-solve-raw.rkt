#lang racket

(require "../math-recurrence-sequence.rkt")

(define intro-msg
  (string-append
   "welcome to my rough equation solver." "\n"
   "this tool operates the calculation of finding the roots of the equation y=f(x)=0 by Newton's method."
   "\n"
   "notice this tool accepts the complete scheme code of the function y=f(x) by defining "
   "it by lambda and operators (like + - * / expt cos etc.)"
   "\n"
   "for example, by typing `(lambda (x) (- x 1))` one can find the root to the equation x - 1 = 0."
   "\n" "\n"
   "the tool provides custom options such as precision (for derivative calculating) and number of recursive calls."
   "but a number that is too small ie (expt 0.1 -20) does not guarantee the precision. the result might look weird "
   "or very incorrect.so an appriopriate precision is important."
   "\n" "\n"))

(define ask-func
  "please enter your function. this starts with (lambda (x) ...)\n")
(define ask-guess
  "enter your initial guess.\n")
(define ask-prec
  "enter your desired precision.\n")
(define ask-recur
  "how many times do you want the procedure to recur?\n")

(define ask-msg
  (string-append
   "\n" "\n"
   "what do you want to do next?" "\n"
   "enter 1: adjust the initial guess, useful for finding other roots." "\n"
   "enter 2: adjust the precision." "\n"
   "enter 3: adjust the number of recursive calls." "\n"
   "enter q: determine another equation." "\n" "\n"))

(define inc-func
  "make sure you typed in the correct format.\n \n")

(define inc-order
  "that is not a valid order.\n \n")
   
(display intro-msg)

(define (newton-solve-application)
  (define ns (make-base-namespace))
  (display ask-func)
  (define f (eval (read) ns))
  (display ask-guess)
  (define guess (read))
  (display ask-prec)
  (define step-differential (read))
  (display ask-recur)
  (define step-recursive (read))
  (if (and (procedure? f) (number? guess)
           (number? step-differential) (number? step-recursive)
           (> step-recursive 0))
      (display ((newton-solve f guess step-differential) step-recursive))
      ((display inc-func) (newton-solve-application)))
 (local
    ((define (break-pt local-guess local-prec local-step)
       (display newtonsolve-ask-msg)
       (define order (read))
       (cond
         ((eq? order '1)
          (display newtonsolve-ask-guess)
          (define new-guess (read))
          (display ((newton-solve f new-guess local-prec) local-step))
          (break-pt new-guess local-prec local-step))
         ((eq? order '2)
          (display newtonsolve-ask-prec)
          (define new-step-differential (read))
          (display ((newton-solve f local-guess new-step-differential) local-step))
          (break-pt local-guess new-step-differential local-step))
         ((eq? order '3)
          (display newtonsolve-ask-recur)
          (define new-step-recursive (read))
          (display ((newton-solve f local-guess local-prec) new-step-recursive))
          (break-pt local-guess local-prec new-step-recursive))
         ((eq? order '4)
          (newton-solve-application))
         ((eq? order 'q)
          (caltool-main))
         (else
          (display caltool-inc-order)
          (break-pt local-guess local-prec local-step)))))
    (break-pt guess step-differential step-recursive)))

(newton-solve-application)
