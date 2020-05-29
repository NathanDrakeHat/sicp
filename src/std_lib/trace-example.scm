(define fast-exp ; recursive version
	(trace-lambda fast-exp (a n) 
		(define (even? n) (= (remainder n 2) 0)) 
		(define (square x) (* x x)) 
		(cond ((= n 0) 1) 
			((even? n) (square (fast-exp a (/ n 2)))) 
			(else (* a (fast-exp a (- n 1)))))))
(define a (fast-exp 2 10)) ; compute when define

(define (fast-expt b n) ; iterative version
	(define fast-expt-iter
		(trace-lambda fast-expt-iter (b n a) 
		(cond ((= n 0) a) 
			((= (remainder n 2) 0) (fast-expt-iter (* b b) (/ n 2) a)) 
			(else (fast-expt-iter b (- n 1) (* a b)))))) 
	(fast-expt-iter b n 1))
(fast-expt 2 10)