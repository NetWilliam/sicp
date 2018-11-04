(define (make-segment sp ep)
  (cons sp ep))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (midpoint-segment seg)
  (make-point
   (/
    (+ (x-point (start-segment seg))
       (x-point (end-segment seg)))
    2.0)
   (/
    (+ (y-point (start-segment seg))
       (y-point (end-segment seg)))
    2.0)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (make-rectangle pa pb) ())
(define (get-width rect) ())
(define (get-height rect) ())
; no much meaning in fulfill these codes, skip

(define (area-of-rectangle rect)
  (* (get-width rect) (get-height rect)))
(define (perimeter-of-rectangle rect)
  (* (+ (get-width rect) (get-height rect)) 2))
