; fold-left and fold-right comparison

(define (my-fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; this is wrong cause the transformation from rest to the result has a reverse effect like push and pop operations of stack
; the example of this error is the difference between `myappend1` and `myappend2`
(define (my-fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter initial sequence))


; fold-left and fold-right will produce the same results if and only if the op satisfy the requirement that x op y = y op x

(define seq1 (list 1 2 3))
(define seq2 (list 4 5 6))
(define (myappend1 seq1 seq2)
  (fold-right cons seq2 seq1))
(define (myappend2 seq1 seq2)
  (my-fold-right cons seq2 seq1))
