; representation of the rational numbers
; closure to represent special data
; symbolic differentiation, the representation of sets, the encoding of information
; generic operations, In particular, we introduce data-directed programming as a technique that allows individual data representations to be designed in isolation and then combined additively
; polynomials' operations

; 2.1 introduction to data abstraction "concrete data" <=== selector, consturctor ===> "abstract data"
; wishful thinking
; pairs cons, car, cdr
; what's more: https://en.wikipedia.org/wiki/CAR_and_CDR
; list-structed data

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* numer y) (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d))
        (pn (if (< d 0) (- n) n))
        (pd (if (< d 0) (- d) d)))
    (cons (/ pn g) (/ pd g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(define minus-one-half (make-rat 1 -2))
(define minus-one-third (make-rat -1 3))
