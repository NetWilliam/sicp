; sicp describe too few for the quote chapter; see https://courses.cs.washington.edu/courses/cse341/04wi/lectures/14-scheme-quote.html
; that strings and symbols are distinct data types in Lisp.

; symbols operations:
; (symbol? x) => x is a symbol?
; (eq? x y) => x and y are the same symbols?
; 'a => symbol `a`
; `(a b c) => symbol (list 'a 'b 'c)

; vs

; (number? x) => x is a number?


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? x1 x2)
  (and (number? x1) (number? x2) (eq? x1 x2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-sum2 a1 a2) '(+ a1 a2))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2) (list '* m1 m2))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (addend s) (cadr s))
;(define (augend s) (caddr s))
;(define (augend s)
;  (cond ((pair? (cddr s)) (make-sum (caddr s) (cdddr s)))
;        (caddr s)))
;(define (augend s) (caddr s))
(define (augend s)
  (cond ((number? (cddr p)) (cddr p))
        ((variable? (cddr p)) (cddr p))
        (else (make-sum (caddr p) (cdddr p)))))
(define (augend s)
  (make-sum-from-list (cddr s)))
(define (make-sum-from-list l)
  (cond ((number? l) l)
        ((null? l) 0)
        (else
         (make-sum (car l) (make-sum-from-list (cdr l))))))

;1 ]=> (deriv '(* x y (+ x 3)) 'x)
;
;;unkown expression type -- DERIV ((+ x 3))
;;To continue, call RESTART with an option number:
;; (RESTART 1) => Return to read-eval-print level 1.
;
;2 error> (restart 1)
;
;1 ]=> (deriv '(* x y (+ x 3) 5) 'x)
;
;;unkown expression type -- DERIV ((+ x 3) 5)
;;To continue, call RESTART with an option number:
;; (RESTART 1) => Return to read-eval-print level 1.

;(define (augend s) ; (cdddr s) must be make-sumed recursively
;  (cond ((= (length (cddr s)) 1) (caddr s))
;        (else (make-sum (caddr s) (cdddr s)))))

(define (multiplier p) (cadr p))
;(define (multiplicand p) (caddr p))
;(define (multiplicand p)
;  (if (pair? (cddr p))
;      (make-product (caddr p) (cdddr p))
;      (caddr p)))
(define (multiplicand p)
  (cond ((number? (cddr p)) (cddr p))
        ((variable? (cddr p)) (cddr p))
        (else (make-product (caddr p) (cdddr p)))))
(define (multiplicand p)
  (make-product-from-list (cddr p)))
(define (make-product-from-list l)
  (cond ((number? l) l)
        ((null? l) 1)
        (else
         (make-product (car l) (make-product-from-list (cdr l))))))
;(define (multiplicand p) (caddr p))
;(define (multiplicand p)   ; error as (augend s)
;  (cond ((= (length (cddr p)) 1) (caddr p))
;        (else (make-product (caddr p) (cdddr p)))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (multiplicand exp)
                                 (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (if (same-variable? (base exp) var)
             (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))
             (make-product (log (base exp)) (make-exponentiation (base exp) (exponent exp)))))
        (else
         (error "unkown expression type -- DERIV" exp))))

; exercise 2.56 and 2.57 here

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? b 0) 0)
        ((and (number? b) (number? e))
         (exp (* e (log b))))
        (else (list '** b e))))


