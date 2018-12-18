(load "example.scm")

; eval in ordinary eval should return 3: (eval '(cond ((> 1 3) 1) (else 3)) (the-environment))
; eval in ordinary eval should return 2: (eval '(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false)) (the-environment))
; eval in example eval should raise an error (eval '(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false)) '())
; let's do it
; eval in example eval should return 3: (eval '(cond ((> 1 3) 1) (else 3)) '())


(define (cond-extension-symbol? symbol)
  (eq? symbol '=>))

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
