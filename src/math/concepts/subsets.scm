(define nil '()) 
  
(define subsets
	(trace-lambda subsets (s)
  	(if (null? s) 
        (list nil) 
        (let ((rest (subsets (cdr s)))) 
          (append rest [map (lambda (x) (append (list (car s)) x)) rest]) ; append former one element
				)
		)
	)
) 
  
(subsets (list 1 10 -5)) ; (1 10 -5 ())
;(()) --> ((-5))
;           |
;       (() (-5)) --> ((10) (10 -5))
;                            |
;	                 (() (-5) (10) (10 -5))  --> ((1) (1 -5) (1 10) (1 10 -5))
;                                                        |
;	                                 (() (-5) (10) (10 -5) (1) (1 -5) (1 10) (1 10 -5))