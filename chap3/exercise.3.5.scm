(load "monte-carlo.scm")

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-intergral p x1 x2 y1 y2 trials)
  (define experiment
    (let ((x (random-in-range x1 x2)) (y (random-in-range y1 y2)))
      (lambda () (p x y))))
  (*
   (monte-carlo trials (lambda () (p (random-in-range x1 x2) (random-in-range y1 y2))))
   (- x2 x1)
   (- y2 y1)))

(define (P x y)
  (< (+
      (square (- x 5))
      (square (- y 7)))
     (square 3)))

; call with (estimate-intergral p 2.0 8.0 4.0 10.0 10000) instead of (estimate-intergral p 2 8 4 10 10000)
