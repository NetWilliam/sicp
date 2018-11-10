; the ability to create pairs whose elements are pairs is the essence of list structure's importance as a representational tool.
; we refer to this ability as the `closure property` of cons;
; in general, an operation for combining data objects satisfies the closure property if the results of combining things with that operation can themselves be combined using the same operation.
; which means for a fun f(a, b), f(c, f(a, b)) is also meaningful.

; such a sequence of pairs, formed by nested conses, is called a list, and Scheme provides a primitive called list to help in constructing lists.
; like: (list 1 2 3 4)

; iterative way: you get the answer caculated at the last call, initial value at the first call stack frame; recursive way: you get the initial value at the last call and the final answer in the latest call


(define (myappend l1 l2)
  (if (null? l1)
      l2
    (cons (car l1) (append (cdr l1) l2))))

(define odds (list 1 3 5 7))
(define squares (list 1 4 9 16 25))

(define (last-pair l)
  (if (null? (cdr l))
      ((lambda (x) (display x) x) l)
    ;l
    (last-pair (cdr l))))

;(define (myreverse mylist) ; wrong
;(if (null? (cdr mylist))
;    (lambda (x) (cons (car mylist) x))
;    (cons (myreverse (cdr mylist)) (car mylist))
;))

(define (myreverse3 mylist) ; wrong
  (if (null? (cdr mylist))
      (lambda (x) (cons (car mylist) x))
    ;((myreverse (cdr m)) (car m))
    ;(cons (myreverse (cdr mylist)) (car mylist))
    (cons (myreverse (cdr mylist)) (car mylist))
    ;(lambda (ml) (myreverse (cdr ml)) (car ml))
    ;(lambda (k) (cons ((myreverse (cdr mylist)) (car mylist)) k))
    )
  )


;(define (myreverse mylist) ; wrong
;  (if (null? (cdr mylist))
;      (cons (car mylist) '())
;    (cons (myreverse (cdr mylist)) (car mylist))
;    )
;  )

(define (myreverse mylist) ; right
  (define (gen-reverse li tail)
    (
     if (null? (cdr li))
     (cons (car li) tail)
     (gen-reverse (cdr li) (cons (car li) tail))
     ))
  (gen-reverse (cdr mylist) (cons (car mylist) '()))
  )

(define (myreverse2 mylist) ; wrong
  (if (null? (cdr mylist))
      (cons (car mylist) '())
    (append (myreverse2 (cdr mylist)) (list (car mylist))) ; right
    ;    (let ((ret (myreverse (cdr mylist)))) ; wrong
    ;      (if (null? (cdr ret))
    ;          (cons (car ret) (car mylist))
    ;        (cons ret (car mylist))
    ;        )
    ;      )
    )
  )
    ;(cons (myreverse (cdr mylist)) (car mylist))
