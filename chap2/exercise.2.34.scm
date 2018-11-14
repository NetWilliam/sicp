(load "example.2.2.3.scm")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define seq (list 1 3 0 5 0 1)); 1 + 3x + 5x^3 + x^5 at x = 2 should be 79
(define x 2)
