(load "example.scm")


; scheme concept alist, assoc, assv and assq refer to:
; https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Association-Lists.html#FOOT12

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (if (= (length vars) 0)
          base-env
          (extend-environment (cdr vars)
                              (cdr vals)
                              (cons (cons (car vars)
                                          (car vals))
                                    base-env)))
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (setup-environment)
  ())

(define the-global-environment (setup-environment))

(define (set-variable-value!))
(define (define-variable!))
(define (looup-variable-value))
