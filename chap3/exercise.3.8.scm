(define (make-f)
  (let ((inner-x 10))
    (define (inner-f x)
      (if (and (not (= inner-x 0)) (not (= inner-x 1)))
          (begin (set! inner-x x) inner-x)
        (if (< inner-x x)
            inner-x
          x)))
    inner-f))
(define f (make-f))

; notice that load with scheme -load exercise.3.8.scm
; (+ (f 0) (f 1)) produce 1
; quite scheme commandline and reopen by the -load command
; (+ (f 1) (f 0)) produce 0
; so the scheme evaluate the parameter from right to left
