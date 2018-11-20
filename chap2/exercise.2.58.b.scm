(load "example.2.3.2.scm")


(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (lengtheq? l s)
  (= (length l) s))

(define (oridinary-exp s) (= (length s) 3))
(define (originary-op-end s) (= (length s) 1))
(define (read-next-op s)
  ;  (cond ((= (cadr s) '+) make-sum)
  ;        ((= (cadr s) '*) make-product))
  (cond ((= (cadr s) '+) '+)
        ((= (cadr s) '*) '*))
  (else
   (error "unknown op type -- READ-NEXT-OP" (cadr s))))



(define (addend s) (car s))
;(define (nextop s) (cddr s))
(define (aguend s)
  (cond ((originary-op-end (cddr s)) (caddr s))
        ((eq? (read-next-op (cddr s) '*))
         (make-product (car s) (multiplicand (cddr s))))
        ((eq? (read-next-op (cddr s) '+)) (make-sum (addend s) (augend s)))))

(define (multiplier p) (car s))
(define (multiplicand p)
  (cond ((originary-op-end (cddr p)) (caddr p))
        ((eq? (read-next-op (cddr p) '*)))))


;(define (addend s) (car s))
;(define (augend s)
;  (cond ((= (length (cddr s)) 1) (caddr s))
;        ;((product? (cddr s)) (make-product (caddr s) (cddddr s)))))
;        ((product? (cddr s)) (make-product-from-list (cddr s)))))
;;(define (augend s) (caddr s))
;
;(define (multiplier p) (car p))
;(define (multiplicand p) (make-product-from-list (cddr p)))
;;(define (multiplicand p) (caddr p))
;
;(define (make-sum-from-list l)
;  (cond ((number? l) l)
;        ((null? l) 0)
;        ((pair? (car l))
;         (cond ((product? (car l)) (make-product-from-list (car l))
;                ((sum? (car l)) (make-sum-from-list (car l))))))
;        ;((product? l) (make-product-from-list l))
;        ((and (>= (length l) 3) (product? (caddr l))) (make-sum (car l) (make-product-from-list (cddr l))))
;        ((<= (length l) 1) (make-sum (car l) 0))
;        (else
;         (make-sum
;          (if (and (list? (car l)) (product? (car l)))
;              (make-product-from-list (car l))
;            (car l))
;          (make-sum-from-list (cddr l))))))
;
;(define (make-product-from-list l)
;  (cond ((number? l) l)
;        ((null? l) 1)
;        ((pair? (car l))
;         (cond ((product? (car l)) (make-product-from-list (car l))
;                ((sum? (car l)) (make-sum-from-list (car l))))))
;        ((<= (length l) 1) (make-product (car l) 1))
;        (else
;         (make-product
;          (if (and (list? (car l)) (sum? (car l)))
;              (make-sum-from-list (car l))
;            (car l))
;          (make-product-from-list (cddr l))))))


