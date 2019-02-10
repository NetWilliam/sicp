(define empty-board '())
(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

(define (adjoin-position row k rest-of-queens)
  (cons row rest-of-queens))

(define (safe? k positions)
  (define (rowsafe? nk position)
    (= (length (filter (lambda (pos) (= pos nk)) position)) 0))
  (define (diagonalsafe? k k-col position col-idx)
    (if (null? position)
        #t
        (and
         (not
          (=
           (abs (- (car position) k))
           (abs (- k-col col-idx))))
         (diagonalsafe? k k-col (cdr position) (+ 1 col-idx)))))
  (let ((new-queen (car positions))
        (old-queens (cdr positions)))
    (if (null? old-queens)
        #t
        (and
         (rowsafe? new-queen old-queens)
         (diagonalsafe?
          new-queen
          1
          old-queens
          2)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
; (length (queens 8)) should be 92: there 92 different solution to `8 queens` problem
