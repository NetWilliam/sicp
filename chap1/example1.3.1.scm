(define (sum term a next b)
  (if (> a b)
      0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc n) (+ n 1))
;(define (cube a term b)
;  (if (> a b)
;      0
;    (+ (* a a a) (cube (term a) term b))))
;(define (sum-cubes a b)
;  sum (cube a inc b))

(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  ;(* (sum f (+ a (/ dx 2.0)) add-dx b) ; more accuracy
  (* (sum f a add-dx b)
     dx))
