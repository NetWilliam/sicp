(load "exercise.4.7.scm")

; a:
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Lookup variable into *unassigned* value! variable:" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; b:
(define (scan-out-defines procedure-body)
  (define (nested-define-name)
    (map definition-variable
         (filter
          definition?
          procedure-body)))
  (define (nested-define-value)
    (map definition-value
         (filter
          definition?
          procedure-body)))
  (define (rest-sequences)
    (remove
     definition?
     procedure-body))
  (if (null? (nested-define-name))
      procedure-body
      (list (append '(let)
                    (list (map (lambda (item) (list item '(quote *unassigned*)))
                               (nested-define-name)))
                    (append (map (lambda (item) (list 'set! (car item) (cadr item)))
                                 (zip (nested-define-name) (nested-define-value)))
                            (rest-sequences))))))
;  (append (list 'let (map (lambda (item) (list item '*unassigned*))
;                          (nested-define-name)))
;          (map (lambda (item) (list 'set! (car item) (cadr item)))
;               (zip (nested-define-name) (nested-define-value)))
;          (rest-sequences)))

; c: scan only once at evaluation
(define (make-procedure parameters body env)
  ;(display "make-procedure is called\n")
  (list 'procedure parameters (scan-out-defines body) env))

(define test-x '((lambda (x) (define a (+ 3 5)) (define b (* 4 6)) (+ a b x)) 3)) ; result of the eval should be 35
(define error-x '((lambda (x) (define (xodd? n) (if (= n 0) false (xeven? (- n 1)))) (define (xeven? n) (if (= n 0) true (xodd? (- n 1)))) (xodd? (- x 1))) 3)) ; eval of this statement should signal an error

;(define (scan-out-defines body)
;  (let ((defined-vars (definitions body)))
;    (if (null? defined-vars)
;        body
;        (list
;         (make-let-seq
;          (unassigned-definitions defined-vars)
;          (unassigned-initialisations defined-vars)
;          (scanned-body body))))))
;(define (definitions exp)
;  (define (scan-iter body definitions-complete)
;    (cond ((null? body) '())
;          ((definition? (car body))
;           (if definitions-complete
;               (error "define cannot appear in an expression context - DEFINITIONS" exp)
;               (cons (car body)
;                     (scan-iter (cdr body) #f))))
;          (else (scan-iter (cdr body) #t))))
;  (scan-iter exp #f))
;(define (make-let-seq unassigned-vars initial-values body)
;  (append (list 'let unassigned-vars)
;          initial-values
;          body))
;(define (unassigned-definitions define-list)
;  (map (lambda (def)
;         (list (definition-variable def)
;               '(quote *unassigned*)))
;       define-list))
;(define (unassigned-initialisations define-list)
;  (map (lambda (def)
;         (list 'set! (definition-variable def)
;               (definition-value def)))
;       define-list))
;(define (scanned-body body)
;  (cond ((null? body) body)
;        ((definition? (car body)) (scanned-body (cdr body)))
;        (else (cons (car body)
;                    (scanned-body (cdr body))))))
