(define (cont-frac n d k)
  (
   if (= k 0)
   (/ (n k) (d k))
   (/ (n k) (+ (d  k) (cont-frac n d (- k 1))))
   ))
;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           k)

(define (phi)
  (
   let ((k 10))
   (cont-frac (lambda (i) 1.0)
              (lambda (i) 1.0)
              k)
   )
  )

(define (tanx x) ; x in radians
  (let ((k 20))
    (cont-frac (lambda (i) (if (= i k) x (* -1.0 (* x x))))
               (lambda (i) (+ (* 2 (- k i)) 1) )
               k
               )))
;(cont-frac (lambda (i) (if (= i 10) x (* x x)))
;           (lambda (i) (- (* 2(- 10 i)) 1))
;           10
;           )
