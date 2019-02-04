(load "exercise.4.16.scm") ; the definition of `let`

;(let ((a 1))
;  (define (f x)
;    (define b (+ a x))
;    (define a 5)
;    (+ a b))
;  (f 10))

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

; in raw scheme
; (define test-x '(let ((a 1)) (define (f x) (define b (+ 1 x)) (define a 5) (+ a b)) (f 10))) would yield the right answer
; (define test-x '(let ((a 1)) (define (f x) (define b (+ a x)) (define a 5) (+ a b)) (f 10))) would signal an error `Premature reference to reserved name: a`

(define test-x '(let ((a 1)) (define (f x) (define b (+ a x)) (define a 5) (+ a b)) (f 10)))
