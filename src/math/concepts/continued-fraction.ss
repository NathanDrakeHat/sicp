(define (cont-frac-recur n d k) ; recursive
	(define (recur i)  
		(if (= i k) 
			[/ (n i) (d i)] 
			[/ (n i) (+ (d i) (recur (+ i 1)))])) 
	[recur 1])
	
(define (cont-frac-iter n d k) 
	(define (iter i res) 
		(cond [(= i k) (iter (- i 1) (/ (n i) (d i)))] 
			[(> i 0) (iter (- i 1) (/ (n i) (+ (d i) res)))] 
			[else res]))
	[iter k 0])
	
(define k 1000)
(display "recursive result: \n")
(cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) k) 
(/ 1 (cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) k))
(display "iterative result: \n")
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) k) ;1/phi  (phi is golden ratio: (1 + sqrt(5))/2 )
(/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) k))