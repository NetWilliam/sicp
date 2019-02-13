; example.2.42.scm n-queen (queens boardsize)
; (original metacircular evaluator)

;$ scheme -load example.scm
;$ (driver-loop)
;$ #input the contents of n-queens.scm
;$ #count time cost

;;;; M-Eval input:
;(runtime)
;
;;;; M-Eval output:
;.05
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
;.45
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
;7.51
;
;;;; M-Eval input:


; (metacircular evaluator with analyze funciton seperated from eval function)
;$ scheme-load exercise.4.22.scm
;$ (driver-loop)
;$ #input the contents of n-queens.scm
;$ #count time cost
