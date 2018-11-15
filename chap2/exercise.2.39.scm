(define x (list 1 2 3))

(define (myreverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(define (myreverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))
