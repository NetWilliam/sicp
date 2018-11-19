(load "example.2.3.2.scm")


(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (addend s) (car s))
(define (augend s) (make-sum-from-list (cddr s)))

(define (multiplier p) (car p))
(define (multiplicand p) (make-product-from-list (cddr p)))

(define (make-sum-from-list l)
  (cond ((number? l) l)
        ((null? l) 0)
        (else
         (make-sum (car l) (make-sum-from-list (cdr l))))))
(define (make-product-from-list l)
  (cond ((number? l) l)
        ((null? l) 1)
        (else
         (make-product (car l) (make-product-from-list (cdr l))))))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
