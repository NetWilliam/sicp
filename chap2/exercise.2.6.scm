(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x)
                (f
                 (
                  (n f)
                  x)
                 )
                )
    )
  )

; church number: https://en.wikipedia.org/wiki/Church_encoding
(define one (lambda (f) (lambda (x) (f x))))
;(define two (lambda (f) (lambda (g) (lambda (x) (f (g x))) )))
(define two (lambda (f) (lambda (x) (f (f x)))))
;
;(define (add a b)
;  (lambda (f) (a (b f))))

(define (inc x) (+ x 1))

(define (seed n)
  (define (dis-n-asterisk n)
    (cond ((= n 0) (display "*"))
          (else (display "*") (dis-n-asterisk (- n 1)))
          ))
  (newline)
  (dis-n-asterisk (- n 1))
  n
  )
