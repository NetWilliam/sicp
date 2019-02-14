; example.2.42.scm n-queen (queens boardsize)
; (original metacircular evaluator)

;$ scheme -load exercise.4.6.scm
;$ (driver-loop)
;$ #input the contents of n-queens.scm
;$ #count time cost

;;;; M-Eval input:
;(runtime)
;
;;;; M-Eval output:
;.1
;
;;;; M-Eval input:
;(length (queens 5))
;
;;;; M-Eval output:
;10
;
;;;; M-Eval input:
;(runtime)
;
;;;; M-Eval output:
;.47
;
;;;; M-Eval input:
;(length (queens 7))
;
;;;; M-Eval output:
;40
;
;;;; M-Eval input:
;(runtime)
;
;;;; M-Eval output:
;7.05
;
;;;; M-Eval input:
;(length (queens 8))
;
;;;; M-Eval output:
;92
;
;;;; M-Eval input:
;(runtime)
;
;;;; M-Eval output:
;37.34
;
;;;; M-Eval input:


; (metacircular evaluator with analyze funciton seperated from eval function)
;$ scheme-load exercise.4.22.scm
;$ (driver-loop)
;$ #input the contents of n-queens.scm
;$ #count time cost

;;;; M-Eval input:
;(runtime)
;
;;;; M-Eval output:
;.12
;
;;;; M-Eval input:
;(length (queens 5))
;
;;;; M-Eval output:
;10
;
;;;; M-Eval input:
;(runtime)
;
;;;; M-Eval output:
;.48
;
;;;; M-Eval input:
;(length (queens 7))
;
;;;; M-Eval output:
;40
;
;;;; M-Eval input:
;(runtime)
;
;;;; M-Eval output:
;4.73
;
;;;; M-Eval input:
;(length (queens 8))
;
;;;; M-Eval output:
;92
;
;;;; M-Eval input:
;(runtime)
;
;;;; M-Eval output:
;23.9
;
;;;; M-Eval input:



;time cost:
;| table-head | q5 | q7 | q8 |
;| old | .37 | 6.58 | 30.29 |
;| new | .36 | 4.25 | 19.17 |
; cut about 50% time off
