(define (my-sqrt x) ; x^2 = y -> x = y / x, average x and y/x to converge 
	(define (square x) 
		(* x x)) 
	(define (improve guess x) 
		(/ [+ guess (/ x guess)] 2)) 
	(define (min-good? guess x) 
		(< [/ (abs [- (improve guess x) guess]) guess] (expt 0.01 10))) 
	(define (max-good? guess x) 
		(< [abs (- [square guess] x)] (expt 0.1 10))) 
	(define (sqrt-iter-min guess x) 
		(if (min-good? guess x) 
			guess 
			[sqrt-iter-min (improve guess x) x])) 
	(define (sqrt-iter-max guess x) 
		(if (max-good? guess x) 
			guess 
			[sqrt-iter-max (improve guess x) x])) 
	(define (abs-diff a b) 
		(abs (- a b))) 
	(let ([for-min (sqrt-iter-min 1.0 x)] [for-max (sqrt-iter-max 1.0 x)]) 
		(if (<= (abs-diff (square for-min) x) (abs-diff (square for-max) x)) 
			for-min 
			for-max)))
	
(my-sqrt (* 9 0.0000000001))
(sqrt (* 9 0.0000000001)) ; language library is better
(my-sqrt (expt 10 20))

(define (close a b) 
	(< [abs (- a b)] 0.00000001))

(define (iterative-imporve first-guess good-enough? imporve) 
	(define [iter res] 
		(let [(next [imporve res])]
			[if (good-enough? res next) 
				next 
				(iter next)])) 
	(iter first-guess))
	
(define (iter-improve-sqrt x)
	(define imporve 
		[lambda (guess) 
		(/ (+ guess [/ x guess]) 2)]) 
	(iterative-imporve 1.0 close imporve))
	
(iter-improve-sqrt 9)