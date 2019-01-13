(load "example.scm")


; scheme concept alist, assoc, assv and assq refer to:
; https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Association-Lists.html#FOOT12

;(define (extend-environment vars vals base-env)
;  (if (= (length vars) (length vals))
;      (if (= (length vars) 0)
;          base-env
;          (extend-environment (cdr vars)
;                              (cdr vals)
;                              (cons (cons (car vars)
;                                          (car vals))
;                                    base-env)))
;      (if (< (length vars) (length vals))
;          (error "Too many arguments supplied" vars vals)
;          (error "Too few arguments supplied" vars vals))))


(define (define-variable! var val env)
  (let ((result (assoc var env)))
    (if (not result)
        (let ((head (cons (car env) (cdr env))))
          (set-car! env (cons var val))
          (set-cdr! env head))
        (set-cdr! result val))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (append (map (lambda (x) (cons (car x) (cadr x))) (zip vars vals)) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (let ((result (assoc var env)))
    (if (not result)
        (error "Unbound variable" var)
        (enclosing-environment result))))

(define (set-variable-value! var val env)
  (let ((result (assoc var env)))
    (if (not result)
        (error "Unbound variable" var)
        (begin (set-cdr! result val)
               env))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
