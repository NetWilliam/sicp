(load "example.2.2.3.scm")
(define x1 (list 1 2 3))
(define x2 (list 1 2 (list 3 4 (list 5 6)) 7 (list 8 9)))
(define (counter-leaves t)
  (accumulate (lambda (x y)
                (+ x y))
              0
              (map (lambda (x)
                     (if (pair? x)
                         (counter-leaves x)
                         1
                       ))
                   t)))

; from sicp 2.2.3
(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; suggested answer:
(define (sa-counter-leaves t)
  (accumulate + 0 (map (lambda(x) 1) (enumerate-tree t))))
