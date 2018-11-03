(define (compose f g) (lambda (x) (f (g x))))
;(define (repeated f nth) (
;                          if (= nth 1)
;                          (lambda (x) (f x))
;                          (lambda (x) (f ((repeated f (- nth 1)) x)))
;                          ))

(define (repeated f nth) (
                          if (= nth 1)
                          (lambda (x) (f x))
                          (compose f (repeated f (- nth 1)))
                          ))
