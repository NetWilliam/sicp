(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (tree-map action tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map action sub-tree)
           (action sub-tree)))
       tree))
(define (square-tree tree) (tree-map (lambda(x) (* x x)) tree))
