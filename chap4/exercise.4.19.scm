(load "exercise.4.6.scm") ; the definition of `let`
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
