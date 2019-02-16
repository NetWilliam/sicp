(load "example.lazy.evaluation.scm")

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; consider p3 which give the same results in both intepreter:
(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))
(define (p3 x)
  (define (p e)
    (display "(e): ")
    (display e)
    (newline)
    x)
  (p (set! x (cons x '(2)))))

; the thunk e will not be evaluated once it's not needed by primitve procedures
; so that's the answer to subproblem a, b, and c
