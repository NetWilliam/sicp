(load "example.nest.mapping.scm")
(define (enumerate-interval low high)
  (if (> low high)
      ()
    (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap
   (lambda(x)
       (map (lambda(y) (list x y))
            (enumerate-interval 1 (- x 1))))
   (enumerate-interval 1 n)))

(define (exercise-prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
