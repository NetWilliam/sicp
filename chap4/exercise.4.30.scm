(load "example.lazy.evaluation.scm")

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))
; consider p3 which give the same results in both intepreter:
(define (p3 x)
  (define (p e)
    (display "(e): ")
    (display e)
    (newline)
    x)
  (p (set! x (cons x '(2)))))

; the thunk e will not be forced once it's not needed by primitve procedures
; so that's the answer to subproblem a, b, and c

; as to question d, the reason a thunk will affect the later value is because there are some set! operations in it, so if we can eliminate the `set!` commands in sequence, we can use the code from text to speed up the evaluation, or we can devise some keywords to help user indicate the side effect of the thunk and thus we can do the right evaluation operation
