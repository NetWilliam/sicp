(define x (list 1 2 3))
(define (subsets s)
  (if (null? s)
      (list ())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda(r) (cons (car s) r)) rest)))))

; problem: s is not a set mathmatically, which may contain duplicates
