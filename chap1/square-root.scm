(define (average x y) (/ (+ x y) 2.0))
(define (improve-guess guess x) (average guess (/ x guess)))
(define (good-enough guess x) (< (abs (- (* guess guess) x)) 0.001))

(define (new-if predicate then-clause else-clause) (
    cond 
        (predicate then-clause)
        (else else-clause)
))

(define (square-root-guess guess x) (
    new-if (good-enough guess x)
    guess
    (square-root-guess (improve-guess guess x) x)
))


(define (square-root x) (square-root-guess 1.0 x))
