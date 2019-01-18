(define (myeven? n) (if (= n 0) #t (myodd? (- n 1))))
(myeven? 5)
(define (myodd? n) (if (= n 0) #f (myeven? (- n 1))))

; both scheme and racket complain about the error
