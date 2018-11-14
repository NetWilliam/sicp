; conventional interfaces
; map, filter are nested function, accumulate should be implemented by user
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
