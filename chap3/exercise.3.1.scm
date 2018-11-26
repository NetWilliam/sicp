(define (make-accumulator initial)
  (lambda (x)
    (begin (set! initial (+ x initial))
           initial)))
