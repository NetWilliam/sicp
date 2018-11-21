(load "ordered_list.scm")

; 2.6.1 and 2.6.2
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set set1 set2)
  (if (or (null? set1))
      set2
    (union-set (cdr set1) (adjoin-set (car set1) set2))))
