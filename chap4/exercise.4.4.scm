(load "example.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (eval-and exp env)
  (define (and-loop vals)
    (if (null? vals)
        #t
        (let ((first (car vals)) (rest (cdr vals)))
             (if (eval first env)
                 (and-loop rest)
                 #f))))
  (and-loop (logic-operands exp)))
(define (eval-or exp env)
  (define (or-loop vals)
    (if (null? vals)
        #f
        (let ((first (car vals)) (rest (cdr vals)))
             (if (eval first env)
                 #t
                 (or-loop rest)))))
  (or-loop (logic-operands exp)))
(define (logic-operator exp) (car exp))
(define (logic-operands exp) (cdr exp))

; example input: (eval '(and true true) '())
; scheme -load example.scm get an error of unbound variable at `and`
; scheme -load exercise.4.4.scm would work
