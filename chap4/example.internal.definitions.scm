(define (f x)
  (define (myeven? n) (if (= n 0) #t (myodd? (- n 1))))
  (define (myodd? n) (if (= n 0) #f (myeven? (- n 1))))
  (if (myeven? x)
      (+ x 1)
      x))

; both scheme and racket complain about the error
