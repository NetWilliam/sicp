(load "example.2.2.3.scm")

(define seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))) ; expect (22 26 30) from (accumulate-n + 0 seqs)
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map (lambda(x) (car x)) seqs))
            (accumulate-n op init (map (lambda(x) (cdr x)) seqs)))))
