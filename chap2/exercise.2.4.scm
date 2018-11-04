(define (mycons x y)
  (lambda (m) (m x y)))

(define (mycar z)
  (z (lambda (p q) p)))

(define (mycdr z)
  (z (lambda (p q) q)))

; mycar/mycdr -> z -> lambda returned by mycons define -> apply this lambda take a function as param, and invoke this function on list (x y) and return the result of the function as the result of the lambda
