(define raw-eval eval)
(load "example.scm")

(define (eval expr env) ((analyze expr) env))

(define (analyze expr)
  (cond ((self-evaluating? expr) (analyze-self-evaluating expr))
        ((quoted? expr) (analyze-quoted expr))
        ((variable? expr) (analyze-variable expr))
        ((assignment? expr) (analyze-assignment expr))
        ((definition? expr) (analyze-definition expr))
        ((if? expr) (analyze-if expr))
        ((lambda? expr) (analyze-lambda expr))
        ((begin? expr) (analyze-sequence (begin-actions expr)))
        ((cond? expr) (analyze (cond->if expr)))
        ((application? expr) (analyze-application expr))
        (else (error "Unknown expression type: ANALYZE" expr))))

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
      (definition-variable! var (vproc env) env)
      'ok)))

(define (analyze-if expr)
  (let ((pproc (analyze (if-predicate expr)))
        (cproc (analyze (if-consequent expr)))
        (aproc (analyze (if-aternative expr))))
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
