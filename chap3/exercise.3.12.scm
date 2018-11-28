(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))

; (define z (append x y)), (cdr x) => (b)
; (define w (append! x y)), (cdr x) => (b c d)

(define (make-circle x)
  (set-cdr! (last-pair x) x)
  x)
