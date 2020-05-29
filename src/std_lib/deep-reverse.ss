(define a (list (list 1 2 3) (list 4 5 6) 3))
a
(display "reverse:\n")
(reverse a)

(define (deep-reverse items) ; continuously cons 
  (define (iter items res) 
    (cond ([null? items]  res) 
      ([not (pair? (car items))]  [iter (cdr items) (cons (car items) res)] ) 
      (else  [iter (cdr items) (cons [iter (car items) '()] res)] )
    )
  ) 
  (iter items '() )
)
(display "deep reverse:\n")
(deep-reverse a)