(load "exercise.4.22.scm")

(define (ambeval expr env succeed fail)
  (analyze expr) env succeed fail)

(define (analyze expr)
  (cond ((self-evaluating? expr) (analyze-self-evaluating expr))
        ((islist? expr) (analyze-list expr))
        ((and? expr) (analyze-and expr))
        ((quoted? expr) (analyze-quoted expr))
        ((variable? expr) (analyze-variable expr))
        ((assignment? expr) (analyze-assignment expr))
        ((definition? expr) (analyze-definition expr))
        ((if? expr) (analyze-if expr))
        ((filter? expr) (analyze-filter expr))
        ((map? expr) (analyze-map expr))
        ((flatmap? expr) (analyze-flatmap expr))
        ((lambda? expr) (analyze-lambda expr))
        ((let? expr) (analyze-let expr))
        ((begin? expr) (analyze-sequence (begin-actions expr)))
        ((cond? expr) (analyze (cond->if expr)))
        ((amb? expr) (analyze-amb expr))
        ((application? expr) (analyze-application expr))
        (else (error "Unknown expression type: ANALYZE" expr))))

(define (amb? expr) (tagged-list? expr 'amb))
(define (amb-choices expr) (cdr expr))
(define (ambeval expr env succeed fail)
  ((analyze expr) env succeed fail))

(define (analyze-self-evaluating expr)
  (lambda (env succeed fail)
    (succeed expr fail)))

(define (analyze-quoted expr)
  (let ((qval (text-of-quotation expr)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable expr)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value expr env) fail)))

(define (analyze-lambda expr)
  (let ((vars (lambda-parameters expr))
        (bproc (analyze-sequence (lambda-body expr))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-if expr)
  (let ((pproc (analyze (if-predicate expr)))
        (cproc (analyze (if-consequent expr)))
        (aproc (analyze (if-alternative expr))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-sequence exprs)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
          (cdr rest-procs))))
  (let ((procs (map analyze exprs)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-definition expr)
  (let ((var (definition-variable expr))
        (vproc (analyze (definition-value expr))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment expr)
  (let ((var (assignment-variable expr))
        (vproc (analyze (assignment-value expr))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value!
                             var old-value env)
                            (fail2)))))
             fail))))

(define (analyze-application expr)
  (let ((fproc (analyze (operator expr)))
        (aprocs (map analyze (operands expr))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       (lambda (arg fail2)
         (get-args
          (cdr aprocs)
          env
          (lambda (args fail3)
            (succeed (cons arg args) fail3))
          fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unkown procedure type: EXECUTE-APPLICATION"
                proc))))

(define (analyze-amb expr)
  (let ((cprocs (map analyze (amb-choices expr))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define input-prompt  ";;; Amb-Eval input:")
(define output-prompt ";;; AMb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
           (newline) (display ";;; Starting a new problem ")
           (ambeval
            input
            the-global-environment
            (lambda (val next-alternative)
              (user-print val)
              (internal-loop next-alternative))
            (lambda ()
              (annouce-output
               (user-print input)
               (driver-loop))))))))
  (internal-loop
   (lambda ()
     (newline) (display ";;; There is no current problem")
     (driver-loop))))
