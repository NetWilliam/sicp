(load "exercise.4.6.scm") ; the definition of `let`
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

; MIT scheme complains:
;Premature reference to reserved name: a
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.
;
;2 error>

; example interpretor give:

;;;; M-Eval input:
;(let ((a 1))
;  (define (f x)
;    (define b (+ a x))
;    (define a 5)
;    (+ a b))
;  (f 10))
;
;;;; M-Eval output:
;16

