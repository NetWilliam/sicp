(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse x)
  (if (pair? x)
      (reverse (map (lambda(k) (deep-reverse k)) x))
    x))
