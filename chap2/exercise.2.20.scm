; +, * and list take arbitrary numbers of arguments
(define (same-parity x . l)
  (define (extract odd l)
    (if (null? l)
        l
      (if (= (modulo (car l) 2) odd)
          (append (list (car l)) (extract odd (cdr l)))
        (extract odd (cdr l))
        )
      )
    )
  (append (list x) (extract (modulo x 2) l))
  )
