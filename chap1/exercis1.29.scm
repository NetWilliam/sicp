; prove of Simpson's Rule see to: https://www.intmath.com/integration/6-simpsons-rule.php
; exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
      (iter (next a) (+ result (term a)))))
  (iter a 0))
;(define (sum term a next b)
;  (if (> a b)
;      0
;    (+ (term a)
;       (sum term (next a) next b))))

; exercise 1.29
(define (cube x) (* x x x))
(define (simpson-integral f a b dx)
  (define (add-2dx x) (+ x dx dx))
  (define (simpson-wrapper x)
    (+
     (f x)
     (* 4 (f (+ x dx)))
     (f (+ x dx dx))))
  (/ (*
      (sum
       simpson-wrapper
       a
       add-2dx
       b)
      dx)
     3)
  )
