(define odds (list 1 3 5 7))
(define squares (list 1 4 9 16 25))

; the use of last-pair might also contribute to the myreverse function in some way

(define (myreverse mylist)
  (if (null? (cdr mylist))
      mylist
    (cons (myreverse (cdr mylist)) (car mylist))
    )
  )

(define (flat mylist) ;((((7) . 5) . 3) . 1) ; different between (append (list 1 2 3) 4) and (append (list 1 2 3) (list 4))
  (if (null? (cdr mylist))))

