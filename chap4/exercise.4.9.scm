(load "exercise.4.8.scm")

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
        ((for? expr) (eval (for->combination expr) env))
        ((begin? expr)
         (eval-sequence (begin-actions expr) env))
        ((cond? expr) (eval (cond->if expr) env))
        ((application? expr)
         (apply (eval (operator expr) env)
                (list-of-values (operands expr) env)))
        (else
         (error "Unknown expression type: EVAL" expr))))


(define (make-assignment symbol value)
  (list 'set! symbol value))
;(for (((i 0) (j 1)) (< (+ i j) 10) ((i (+ i 1)))) (display "i:") (display i) (display "\n") (display "j:") (display j) (display "\n"))
(define (for? expr) (tagged-list? expr 'for))
(define (for-statement expr) (cadr expr))
(define (for-body expr) (cddr expr))
(define (for-initial expr) (car (for-statement expr)))
(define (for-initial-symbols expr) (map car (for-initial expr)))
(define (for-initial-values expr) (map cadr (for-initial expr)))
(define (for-predicate expr) (cadr (for-statement expr)))
(define (for-update expr) (caddr (for-statement expr)))

(define (update->set expr)
  (define (get-set set-pairs)
    (if (null? set-pairs)
        '()
        (append
         (get-set (cdr set-pairs))
         (cons (make-assignment (car (car set-pairs))
                                (cadr (car set-pairs)))
               '())
         )))
  (get-set (for-update expr)))

(define (init->define expr)
  (define (get-define define-pairs)
    (if (null? define-pairs)
        '()
        (append
         (get-define (cdr define-pairs))
         (cons (list 'define (car (car define-pairs))
                     ;(sequence->exp (cdr (car define-pairs))))
                     (cadr (car define-pairs)))
               '()))))
  (get-define (for-initial expr)))

(define (content->define expr)
  (list (list 'define (list 'inner-loop)
              (list 'if (for-predicate expr)
                    (sequence->exp (append
                                    (append (for-body expr)
                                            (update->set expr))
                                    (cons '(inner-loop) '())))))
        (list 'inner-loop)))

(define (for->combination expr)
  (cons
   (make-lambda
    '()
    (append (init->define expr) (content->define expr)))
   '()))

(define test
  '(for (((i 0) (j 1)) (< (+ i j) 10) ((i (+ i 1)))) (display "i:") (display i) (display "\n") (display "j:") (display j) (display "\n")) )
