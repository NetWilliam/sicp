(load "example.2.2.3.scm")
(load "exercise.2.36.scm")

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define v (list 1 3 5 7))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n (lambda(x y) (append (list x) y)) () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda(row) (matrix-*-vector cols row)) m)))
