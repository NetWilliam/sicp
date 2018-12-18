(load "example.scm")

; eval in ordinary eval should return 3: (eval '(cond ((> 1 3) 1) (else 3)) (the-environment))
; eval in ordinary eval should return 2: (eval '(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false)) (the-environment))
; eval in example eval should raise an error (eval '(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false)) '())
; let's do it
; eval in example eval should return 3: (eval '(cond ((> 1 3) 1) (else 3)) '())

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((gt? exp) (eval-gt exp env))
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

(define (gt? exp)
  (eq? (operator exp) '>))
(define (eval-gt exp env)
  (> (cadr exp) (caddr exp)))

(define (cond-extension-symbol? symbol)
  (eq? symbol '=>))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
