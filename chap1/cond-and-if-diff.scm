(define (p) (p))

(define (cond_test x) (
    cond ((= x 0) 0)
    (else (p))
))

(define (if_test x) (
    if (= x 0) 0 (p)
))
