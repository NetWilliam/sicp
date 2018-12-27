(load "exercise.4.7.scm")

(define (named-let? expr)
  (symbol? (cadr expr)))
(define (let-name expr)
  (cadr expr))

(define (let-body expr)
  (if (named-let? expr)
      (cdddr expr)
      (cddr expr)))
(define (let-pairs expr)
  (if (named-let? expr)
      (caddr expr)
      (cadr expr)))

(define (let->combination expr)
  (if (named-let? expr)
      (cons (make-lambda '()
                         (list
                          (list 'define (let-name expr) (make-lambda
                                                         (let-vars expr)
                                                         (let-body expr)))
                          (cons (let-name expr) (let-exps expr))))
            '())
      (cons (make-lambda (let-vars expr) (let-body expr))
            (let-exps expr))))

(define test '(define (fib n)
               (let fib-iter ((a 1)
                              (b 0)
                              (count n))
                 (if (= count 0)
                     b
                     (fib-iter (+ a b) a (- count 1))))))
(define tl
  '(let fib-iter ((a 1)
                  (b 0)
                  (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))
