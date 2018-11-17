(load "example.nest.mapping.scm")

(define empty-board '())

;(define (adjoin-position row k rest-of-queens)
;  (if (null? rest-of-queens)
;      (list 1)
;    ()))
(define (adjoin-position row k rest-of-queens)
  (cons row rest-of-queens))

; the respect of queens requires their not being in the same rows, cols and diagnols
; cols are naturally different, all we need is to check the rows and diagnols
; wrong safe
;(define (safe? k positions)
;  (define (rowsafe? k position)
;    (= (length (filter (lambda (pos) (= pos k)) position))) 0)
;  (define (diagonalsafe? k k-col position col-idx)
;    (if (null? position)
;        #t
;      (and
;       (not
;        (=
;         (abs (- (car position) k))
;         (abs (- k-col col-idx))))
;       (diagonalsafe? k k-col (cdr position) (+ 1 col-idx)))))
;  (and (rowsafe? k positions) (diagonalsafe? k (+ (length positions) 1) positions 1)))
;  (map (lambda (x)
;         (and
;          (rowsafe? k x)
;          (diagonalsafe? k (+ (length x)) x 1)))
;       positions))

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

; (length (queens 8)) should be 92: there 92 different solution to `8 queens` problem
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
