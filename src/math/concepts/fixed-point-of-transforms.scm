(define (fixed-point f first-guess tolerance) 
	(define [enough? a b] 
		[< (abs [- a b]) tolerance]) 
	(define [iter res] 
		[if (enough? res [f res]) 
			[f res] 
			(iter [f res])]) 
	(iter first-guess)) 

(define (newton-transform g) ; x - g(x)/g'(x)
	(define (deriv g) 
		(lambda (x) 
			(let [(dx 0.0000001)] 
				[/ (- [g (+ x dx)] (g x)) dx]))) 
	(lambda (x) 
		[- x (/ [g x] [(deriv g) x])]))
		
(define (average-damp f) 
	(lambda [x] [/ (+ x [f x]) 2]))
	
(define (fixed-point-of-transforms f transform guess) 
	(fixed-point [transform f] guess 0.0000001))
	
(define (sqrt-one x) 
	(fixed-point-of-transforms [lambda (v) (/ x v)] average-damp 1.0))

(define (sqrt-two x) 
	(fixed-point-of-transforms [lambda (v) (- [* v v] x)] newton-transform 1.0))
	
(sqrt-one 9)
(sqrt-two 9)