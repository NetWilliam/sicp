(define (is-odd x) (
    if (= (modulo x 2) 0)
    ((lambda(k) (display k) (display " is not odd number") false) x) ; strings must be quoted by double quotes instead of quotes
    true
))
