(define (count-pairs x)
  (if (not (pair? x))
      0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define x (cons 1 3)) ; (count-pairs (cons (cons (cons x '())))) => 3
(define y (cons x x)) ; (count-pairs (cons y 1)) => 4
(define z (cons y y)) ; (count-pairs z) => 7
; loop never returned
