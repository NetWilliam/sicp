(load "example.2.2.3.scm")
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

; (flatmap (list 1 2 3)) reduce an error, why?
; hint: append take two different input

(define (prime? x)
  (let ((root (sqrt x)))
    (define (test-prime x n)
      (if (>= n root)
          (not (= 0 (modulo x n)))
        (and (not (= 0 (modulo x n)))
             (test-prime x (+ n 1)))))
    (test-prime x 2)))
;(define (prime? x)
;  (define (test divisor)
;    (cond ((> (* divisor divisor) x) #t)
;          ((= 0 (remainder x divisor)) #f)
;          (else (test (+ divisor 1)))))
;  (test 2))


;(define (enumerate-interval interval end) ; false demonstration here, some terminating situation would cause `maximum recursion depth exceeded` error
;  (if (= 1 end)
;      (list end)
;    (append (enumerate-interval interval (- end interval)) (list end))))
(define (enumerate-interval n1 n2)
  (define (enumerate-interval-iter start stop current)
    (if (< stop start)
        '()
      (if (= start stop)
          (append current (list start))
        (enumerate-interval-iter (+ 1 start) stop (append current (list start))))))
  (enumerate-interval-iter n1 n2 '()))


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
