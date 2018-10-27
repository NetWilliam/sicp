(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q)) ;p'
                   (+ (* 2 p q) (* q q)) ;q'
                   (/ count 2)))
        (else (fib-iter
               (+ (* b q) (* a q) (* a p))
               (+ (* b p) (* a q))
               p
               q
               (- count 1)))
        ))
;$$
;\left[
;\begin{matrix}
;p+q & q \\
;q & p
;\end{matrix}
;\right]
;$$
;*
;$$
;\left[
;\begin{matrix}
;a \\ b
;\end{matrix}
;\right ]
;$$
;=
;$$
;\left[
;\begin{matrix}
;bq+aq+ap \\
;bp+aq
;\end{matrix}
;\right]
;$$
; https://latex.codecogs.com/gif.latex?$$\left[\begin{matrix}p&plus;q&space;&&space;q&space;\\&space;q&space;&&space;p&space;\end{matrix}&space;\right]&space;\tag{3}&space;$$&space;*&space;$$&space;\left[&space;\begin{matrix}&space;a&space;\\&space;b&space;\end{matrix}&space;\right&space;]&space;$$&space;=&space;$$&space;\left[&space;\begin{matrix}&space;bq&plus;aq&plus;ap&space;\\&space;bp&plus;aq&space;\end{matrix}&space;\right]&space;$$
;$$
; \left[
; \begin{matrix}
;   p+q & q \\
;   q & p
;  \end{matrix}
;  \right]
;$$
;*
;$$
; \left[
; \begin{matrix}
;   p+q & q \\
;   q & p
;  \end{matrix}
;  \right]
;$$
;=
;$$
; \left[
; \begin{matrix}
;   p^{2}+2q^{2}+2pq & q^{2}+2pq \\
;   q^{2}+2pq & p^{2} + q^{2}
;  \end{matrix}
;  \right]
;$$
;https://latex.codecogs.com/gif.latex?$$&space;\left[&space;\begin{matrix}&space;p&plus;q&space;&&space;q&space;\\&space;q&space;&&space;p&space;\end{matrix}&space;\right]&space;$$&space;*&space;$$&space;\left[&space;\begin{matrix}&space;p&plus;q&space;&&space;q&space;\\&space;q&space;&&space;p&space;\end{matrix}&space;\right]&space;$$&space;=&space;$$&space;\left[&space;\begin{matrix}&space;p^{2}&plus;2q^{2}&plus;2pq&space;&&space;q^{2}&plus;2pq&space;\\&space;q^{2}&plus;2pq&space;&&space;p^{2}&space;&plus;&space;q^{2}&space;\end{matrix}&space;\right]&space;$$
