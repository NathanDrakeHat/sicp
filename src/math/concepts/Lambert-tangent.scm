(define (Lambert-tangent x k) 
	(define (recur i) 
		(cond [(= i k) (/ (* x x) (- (* 2 i) 1))] 
			[(= i 1) (/ x (- 1 (recur (+ i 1))))] 
			[else (/ (* x x) (- [- (* 2 i) 1] (recur (+ i 1))))]))
	[recur 1]) ; x is radian
(define pi (acos -1)))
(Lambert-tangent (/ pi 4) 1000)