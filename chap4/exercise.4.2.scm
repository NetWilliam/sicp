(load "example.scm")


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (application? expr) (tagged-list? expr 'call))
(define (operator expr) (cadr expr))
(define (operands expr) (cddr expr))
;(eval '(call + (call (lambda (x) (call * x x)) 3) 1) the-global-environment) => Value: 10
