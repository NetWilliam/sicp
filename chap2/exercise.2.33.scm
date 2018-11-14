; from example.2.2.3.scm
(load "example.2.2.3.scm")

(define (mymap p sequence)
  (accumulate (lambda(x y) (cons (p x) y)) () sequence))

(define (myappend seq1 seq2)
  (accumulate cons seq2 seq1))

(define (mylength sequence)
  (accumulate (lambda(x y) (+ 1 y)) 0 sequence))
