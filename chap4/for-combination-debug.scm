((lambda ()
   (define j 1)
   (define i 0)
   (define inner-loop
     (if (< (+ i j) 10)
         (begin (display "i:")
                (display i)
                (display "\n")
                (display "j:")
                (display j)
                (display "\n")
                (inner-loop))
         ()))
   (inner-loop)))

((lambda ()
   (define j 1)
   (define i 0)
   (define inner-loop
     (if (< (+ i j) 10)
         (begin (display "i:")
                (display i)
                (display "\n")
                (display "j:")
                (display j)
                (display "\n")
                (set! i (+ i 1))
                (inner-loop))
         ()))
   (inner-loop)))
((lambda ()
   (define j 1)
   (define i 0)
   (define inner-loop
     (if (< (+ i j) 10)
         (begin (display "i:")
                (display i)
                (display "\n")
                (display "j:")
                (display j)
                (display "\n")
                (set! i (+ i 1))
                (inner-loop))))
   (inner-loop)))
((lambda ()
   (define j 1)
   (define i 0)
   (define (inner-loop)
     (if (< (+ i j) 10)
         (begin (display "i:")
                (display i)
                (display "\n")
                (display "j:")
                (display j)
                (display "\n")
                (set! i (+ i 1))
                (inner-loop))))
   (inner-loop)))

((lambda ()
   (define i 1)
   (define (inner-loop)
     (if (< i 10)
         (begin (for (((j i)) (< j 10) ((j (+ j 1))))
                     (display i)
                     (display " * ")
                     (display j)
                     (display " = ")
                     (display (* i j))
                     (display "\t"))
                (display "\n")
                (set! i (+ i 1))
                (inner-loop))))
   (inner-loop)))