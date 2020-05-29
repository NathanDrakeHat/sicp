(define (fast-exp a n) ; recursive version
	(cond [(= n 0) 1] 
		[even? n) ((lambda (v) (* v v)) (fast-exp a (/ n 2)))] 
		[else (* a [fast-exp a (- n 1)])]))
(fast-exp 2 12)

(define (fast-expt b n) ; iterative version
	(define (fast-expt-iter b n a) 
		(cond [(= n 0) a] 
			[(= (remainder n 2) 0) (fast-expt-iter (* b b) (/ n 2) a)] 
			[else (fast-expt-iter b [- n 1] [* a b])])) 
	(fast-expt-iter b n 1))
(fast-expt 2 12)
(* 9 (fast-expt 0.1 10))
(* 9 [expt 0.1 10]) ; language library is better