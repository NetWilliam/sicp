(load "ordered_list.scm")

; 2.6.1 and 2.6.2
(define (adjoin-set x set)
  (if ))

(define (union-set set1 set2)
  (if (or (null? set1))
      set2
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (union-set (cdr set1)
                                   (cdr set2))))
              ((< x1 x2)
               (union-set (cdr set1) set2))
              ((< x2 x1)
               (union-set set1 (cdr set2)))))))
