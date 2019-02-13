(define raw-eval eval)
(load "example.scm")

(define (eval expr env) ((analyze expr) env))

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
        ((begin? expr) (analyze-sequence (begin-actions expr)))
        ((cond? expr) (analyze (cond->if expr)))
        ((application? expr) (analyze-application expr))
        (else (error "Unknown expression type: ANALYZE" expr))))

(define (analyze-filter expr)
  (let ((proc (analyze (cadr expr)))
        (fval (analyze (caddr expr))))
    (lambda (env)
      (filter
       (lambda (item)
         (execute-application
          (proc env)
          (list item)))
       (fval env)))))
(define (analyze-map expr)
  (let ((proc (analyze (cadr expr)))
        (mval (analyze (caddr expr))))
    (lambda (env)
      (map
       (lambda (item)
         (execute-application
          (proc env)
          (list item)))
       (mval env)))))
(define (analyze-flatmap expr)
  (let ((proc (analyze (cadr expr)))
        (fval (analyze (caddr expr))))
    (lambda (env)
      (flatmap
       (lambda (item)
         (execute-application
          (proc env)
          (list item)))
       (fval env)))))

(define (analyze-and expr)
  (let ((and-sequence (cdr expr)))
    (if (null? and-sequence)
        (error "ANALYZE and sequence meet a blank and-sequence")
        (let ((sequence (map analyze and-sequence)))
          (define (analyze-and-sequence env seq)
            (if (null? seq)
                #t
                (and ((car seq) env)
                     (analyze-and-sequence env (cdr seq)))))
          (lambda (env) (analyze-and-sequence env sequence))))))

(define (analyze-list expr)
  (let ((list-item (map analyze (cdr expr))))
    (lambda (env) (map (lambda (item) (item env)) list-item))))

(define (analyze-self-evaluating expr)
  (lambda (env) expr))

(define (analyze-quoted expr)
  (let ((qval (text-of-quotation expr)))
    (lambda (env) qval)))

(define (analyze-variable expr)
  (lambda (env) (lookup-variable-value expr env)))

(define (analyze-assignment expr)
  (let ((var (assignment-variable expr))
        (vproc (analyze (assignment-value expr))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition expr)
  (let ((var (definition-variable expr))
        (vproc (analyze (definition-value expr))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if expr)
  (let ((pproc (analyze (if-predicate expr)))
        (cproc (analyze (if-consequent expr)))
        (aproc (analyze (if-alternative expr))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))

(define (analyze-lambda expr)
  (let ((vars (lambda-parameters expr))
        (bproc (analyze-sequence (lambda-body expr))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exprs)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
          (cdr rest-procs))))
  (let ((procs (map analyze exprs)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application expr)
  (let ((fproc (analyze (operator expr)))
        (aprocs (map analyze (operands expr))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error "Unkown procedure type: EXECUTE-APPLICATION"
                proc))))
