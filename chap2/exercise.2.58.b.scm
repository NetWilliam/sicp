(load "example.2.3.2.scm")


(define (sum? x)
  (and (pair? x) (memq '+ x)))
(define (product? x)
  (and (pair? x) (and (not (sum? x)) (memq '* x))))

(define (split-at l x)
  (define (split b e x)
    (if (null? e)
        (list b e)
        (if (eq? (car e) x)
            (list b (cdr e))
            (split (append b (list (car e))) (cdr e) x))))
  (split () l x))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (single-term l)
  (= (length l) 1))
(define (simple-term x)
  (if (single-term x) (car x) x))

(define (addend s) (simple-term (car (split-at s '+))))
(define (augend s) (simple-term (cadr (split-at s '+))))

(define (multiplier p) (simple-term (car (split-at p '*))))
(define (multiplicand p) (simple-term (cadr (split-at p '*))))
