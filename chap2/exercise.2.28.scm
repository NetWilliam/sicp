(define x (list (list 1 2) (list 3 4)))

(define (fringe x)
  (if (pair? x)
      (if (null? (cdr x))
          (fringe (car x))
        (append (fringe (car x)) (fringe (cdr x))))
    (list x)
    ))
;      (if (pair? (car x))
;          (append (fringe (car x)) )
;        x
;        )
