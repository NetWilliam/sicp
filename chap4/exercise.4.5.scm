(load "example.scm")

; eval in ordinary eval should return 3: (eval '(cond ((> 1 3) 1) (else 3)) (the-environment))
; eval in ordinary eval should return 2: (eval '(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false)) (the-environment))
; eval in example eval should raise an error (eval '(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false)) '())
; let's do it
; eval in example eval should return 3: (eval '(cond ((> 1 3) 1) (else 3)) '())


(define (cond-clause-with-extension? clause)
  (memq '=> clause))
(define (cond-action-with-extension clause) (cddr clause))
(define (cond-predicate-with-extension clause) (car clause))
(define (cond-recipient-with-extension clause) (caddr clause))

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
            (if (cond-clause-with-extension? first)
                (make-if (cond-predicate-with-extension first)
                         ;(sequence->exp (cond-action-with-extension first))
                         ;((cond-action-with-extension first) ((cond-predicate-with-extension first)))
                         ;(list (sequence->exp (cond-action-with-extension first)) (cond-predicate-with-extension first)) ; building AST, building structure to be evaluate by eval function
                         (list (cond-recipient-with-extension first) (cond-predicate-with-extension first)) ; both work for (cond ((assoc 'b '((a 1) (b 2))) => (lambda (x) (cadr x))) (else false)) why?
                         ; the designed language happens to have the same lisp syntax with the designing language, so we place the recipient function into a list, it could evaluate normally. but if we are build this script with another language, it probably would cause an error.
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))
