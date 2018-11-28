; http://web.mit.edu/qichaohu/www/6001Projects/lecturenotes/lecture13.pdf
; define rule shadow the unbound variable, set! rule overwrite the unbound variable


; Frame: a table of bindings
; Environment: a sequence of frames
;
; Evaluation in the environment model
; * All evaluation occurs in an environment
;   ** The current environment changes when the interpreter applies a procedure; "a function call in C++"
; * The top environment is called the globale environment(GE)
;   ** Only the GE has no enclosing environment
; * To evaluate a combination
;   ** Evaluate the subexpressions in the current environment
;   ** Apply the value of the first to the values of the rest
;
; ###4 steps to apply a compound procedure P to arguments
; 1. create a new frame A
; 2. Make A into an environment E:
;   A's enclosing environment pointer goes to the same frame as the environment point of P
; 3. In A, bind the parameters of P to the argument values
; 4. Evaluate the body of P with the E as the current environment
