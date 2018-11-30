(define count-pairs
  (let ((symbols '()))
    (lambda (x)
      (if (or (not (pair? x)) (memq x symbols))
          0
        (begin (set! symbols (cons x symbols))
               (+ (count-pairs (car x))
                  (count-pairs (cdr x))
                  1))))))

(define x (cons 1 3))
(define y (cons x x))
(define z (cons y y))
