(define (make-agenda)())

(define (empty-agenda? agenda)())
(define (first-agenda-item agenda)())
(define (remove-first-agenda-item!)())
(define (add-to-agenda! time action agenda)())
(define (current-time agenda)())

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
    (let ((first-item (firts-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propogate))))

