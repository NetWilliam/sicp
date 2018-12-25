(load "exercise.4.6.scm")

(define (eval expr env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (text-of-quotation expr))
        ((assignment? expr) (eval-assignment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((lambda? expr) (make-procedure (lambda-parameters expr)
                                        (lambda-body expr)
                                        env))
        ((let? expr) (eval (let->combination expr) env))
        ((let*? expr) (eval (let*->nested-lets expr) env))
        ((begin? expr)
         (eval-sequence (begin-actions expr) env))
        ((cond? expr) (eval (cond->if expr) env))
        ((application? expr)
         (apply (eval (operator expr) env)
                (list-of-values (operands expr) env)))
        (else
         (error "Unknown expression type: EVAL" expr))))

(define (let->combination expr)
  (cons (make-lambda (let-vars expr) (let-body expr))
        (let-exps expr)))
(define (let*? expr) (tagged-list? expr 'let*))
(define (make-let let-pairs let-body)
  (list 'let let-pairs (sequence->exp let-body)))
(define (make-let* let-pairs let-body)
  (list 'let* let-pairs (sequence->exp let-body)))

(define (let*->nested-lets-right exp)
  (define (make-let params)
    (cond ((last-exp? params) (append (list 'let
                                            (list (car params)))
                                      (let-body exp)))
          (else (list 'let
                      (list (car params))
                      (make-let (cdr params))))))
  (make-let (cadr exp)))

(define (let*->nested-lets expr)
  (if (<= (length (let-pairs expr)) 1)
      (make-let
       (let-pairs expr)
       (let-body expr))
      (make-let
       (list
        (car (let-pairs expr)))
       (list
        (let*->nested-lets
         (make-let*
          (cdr (let-pairs expr))
          (let-body expr)))))))

(define nl-wrong (let*->nested-lets       '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))))
(define nl-right (let*->nested-lets-right '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))))

(define (equal-part? s1 s2)
  (cond ((and (pair? s1) (pair? s2))
         (and (equal-part? (car s1) (car s2))) (equal-part? (cdr s1) (cdr s2)))
        (else (if (equal? s1 s2)
                  #t
                  (begin (display "s1 s2 not equal" s1 s2) #f)))))

;(define (let*-body expr) (caddr expr))
;(define (let*-inits expr) (cadr expr))
;(define (let*->nested-lets expr)
;  (let ((inits (let*-inits expr))
;        (body (let*-body expr)))
;    (define (make-lets exprs)
;      (if (null? exprs)
;          body
;          (list 'let (list (car exprs)) (make-lets (cdr exprs)))))
;    (make-lets inits)))

(eval '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)) the-global-environment)
