(load "example.scm")

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
    (let ((first-val (eval (first-operand exps) env)))
      (cons (fisrt-val (list-of-values-left-to-right exps env))))))

(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
    (let ((rest-of-values (list-of-values-right-to-left exps env)))
      (cons ((eval (first-operand exps) env) rest-of-values)))))
