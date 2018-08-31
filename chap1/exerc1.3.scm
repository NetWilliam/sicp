(define (head-is-the-least x y z)
    (if (and (< x y) (< x z)) true false))

(define (square-sum x y)
    (+ (* x x) (* y y)))

(define (square-sum-of-two-bigger-num-out-of-three x y z)
    (cond
        ((head-is-the-least x y z) (square-sum y z))
        ((head-is-the-least y x z) (square-sum x z))
        (else (square-sum x y))
    )
)

(square-sum-of-two-bigger-num-out-of-three 2 3 4)
