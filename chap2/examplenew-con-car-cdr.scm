(define (mycons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m)))))

(define (mycar z) (z 0))
(define (mycdr z) (z 1))
; procedural representations of data will play a central role in our programming repertoire.
; This style of programming is often called `message passing`
