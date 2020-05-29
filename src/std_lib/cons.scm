(define (cons x y)
  (lambda (select-func) (select-func x y)))

(define (car cons-obj) ; message passing
  (cons-obj [lambda (p q) p])) ; select-func is [lambda (p q) p]

(define (cdr cons-obj)
  (cons-obj [lambda (p q) q]))

(car (cons 1 2))
(cdr (cons 1 2))



(define (cons x y) ; 2^a*3^b
  (* (expt 2 x) (expt 3 y)))
(define (car z)
  (define (recursor i)
    (if [= (remainder z [expt 2 i]) 0]
        [recursor (+ i 1)] 
        [- i 1])) 
  [recursor 0])
(define (cdr z)
  (define (recursor i)
    (if [= (remainder z [expt 3 i]) 0]
        [recursor (+ i 1)]
        [- i 1])) 
  [recursor 0])

(car (cons 34 54))
(cdr (cons 31 22))