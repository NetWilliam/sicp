;(define count-pairs
;  (let ((symbols '()))
;    (lambda (x)
;      (if (or (not (pair? x)) (memq x symbols))
;          0
;        (begin (set! symbols (cons x symbols))
;               (+ (count-pairs (car x))
;                  (count-pairs (cdr x))
;                  1))))))


;(define (count-pairs x)
;  (let ((symbols '()))
;    (define (do-count x)
;      (if (or (not (pair? x)) (memq x symbols))
;          0
;        (begin (set! symbols (cons x symbols))
;               (+ (do-count (car x))
;                  (do-count (cdr x))
;                  1))))
;    (do-count x)))
;
;(define count-pairs ; wrong, two function calls will share the same local vairable `seen`
;  (let ((seen '()))
;    (lambda (x)
;      (cond ((not (pair? x)) 0)
;            ((memq x seen) 0)
;            (else (set! seen (cons x seen))
;                  (+ (count-pairs (car x))
;                     (count-pairs (cdr x))
;                     1))))))

(define (count-pairs x)
  (define (get-counter)
    (let ((symbols '()))
      (define (do-count p)
        (if (or (not (pair? p)) (memq p symbols))
            0
          (begin (set! symbols (cons p symbols))
                 (+ (do-count (car p))
                    (do-count (cdr p))
                    1))))
      do-count))
  ((get-counter) x))

(define x (cons 1 3))
(define y (cons x x))
(define z (cons y y))
