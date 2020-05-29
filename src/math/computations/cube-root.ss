(define (cube-root x) ; generalization of Newton-method
	(define (improve guess) 
		(/ (+ (* 2 guess) (/ x (* guess guess))) 3))
	(define (good? guess) 
		(< (abs (- (* guess guess guess) x))
			0.00001))
	(define (cube-iter guess) 
		(if (good? guess) 
		guess 
		(cube-iter (improve guess))))
	(cube-iter 1.0))
(cube-root 27)