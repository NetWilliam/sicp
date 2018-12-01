(load "queue.scm")

(define (print-queue queue)
  (if (empty-queue? queue)
      '()
    (front-ptr queue)))
