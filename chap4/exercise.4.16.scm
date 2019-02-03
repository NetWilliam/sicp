(load "exercise.4.7.scm")

; a:
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((equal? var '*unassigned*)
             (error "Lookup variable into *unssigned* value!" var))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; b:
(define (scan-out-defines procedure-body)
  (define (is-nested-define item) (eq? (car item) 'define))
  (define (nested-define-name)
    (map cadr
         (filter
          is-nested-define
          procedure-body)))
  (define (nested-define-value)
    (map caddr
         (filter
          is-nested-define
          procedure-body)))
  (define (rest-sequences)
    (remove
     is-nested-define
     procedure-body))
  (make-let (map (lambda (item) (list item '*unassigned*))
                 (nested-define-name))
            (append (map (lambda (item) (list 'set! (car item) (cadr item)))
                         (zip (nested-define-name) (nested-define-value)))
                    (rest-sequences))))
;  (append (list 'let (map (lambda (item) (list item '*unassigned*))
;                          (nested-define-name)))
;          (map (lambda (item) (list 'set! (car item) (cadr item)))
;               (zip (nested-define-name) (nested-define-value)))
;          (rest-sequences)))

; c: scan only once at evaluation
(define (make-procedure parameters body env)
  (display "make-procedure is called")
  (list 'procedure parameters (scan-out-defines body) env))

(define x '((lambda (x) (define a (+ 3 5)) (define b (* 4 6)) (+ a b x)) 3))
(define error-x '((lambda (x) (define xodd? ))))

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
