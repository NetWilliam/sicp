(load "example.nest.mapping.scm")
(define (enumerate-interval low high)
  (if (> low high)
      ()
    (cons low (enumerate-interval (+ low 1) high))))

(define (gen-tuple-sum n s)
  (flatmap
   (lambda (i)
     (filter
      (lambda (l) (= (+ (car l) (cadr l)) s))
      (map
       (lambda (j) (list i j))
       (enumerate-interval 1 n))))
   (enumerate-interval 1 n)))

(define (gen-triple-sum n s) ; this function can be applied more generic
  (flatmap
   (lambda (i)
     (filter
      (lambda (triple)
        (= s (fold-right + 0 triple)))
      (map
       (lambda (tuple)
         (append (list i) tuple))
       (gen-tuple-sum n (- s i)))))
   (enumerate-interval 1 n)))

(define (gen-multi-sum n s c) ; this function still can be implemented via more generic way, but I'm not gonna to do it
  (if (= c 2)
      (flatmap
       (lambda (i)
         (filter
          (lambda (l) (= (fold-right + 0 l) s))
          (map
           (lambda (j) (list i j))
           (enumerate-interval 1 n))))
       (enumerate-interval 1 n))
    (flatmap
     (lambda (newk)
       (map
        (lambda (shortlist)
          (append (list newk) shortlist))
        (gen-multi-sum n (- s newk) (- c 1))))
     (enumerate-interval 1 n))))

