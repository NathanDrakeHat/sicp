; fermat-test is a algorithm to test prime number.
(define (fermat-test n) 
	(define (expmod a p m) ; get a^p mod m. recursive definition
		(cond [(= p 0) 1] ; base case
			[(even? p) (remainder ((lambda (v) (* v v)) (expmod a (/ p 2) m)) m)] ;(a^p % m)^2 % m == a^2p % m
			[else (remainder (* a (expmod a (- p 1) m)) m)])) 
	(define (rand-expmod-test r p m) 
		(= (expmod r p m) r)) ; prime: a^p mod p == a 
	
	(rand-expmod-test [+ 1 (random [- n 1])] n n))

(fermat-test 1999)
(display "\nbelow should be #f\n")
(fermat-test 19999)
(fermat-test 561) ;fermat test can be fooled!
(fermat-test 1105)
(fermat-test 1729)
(fermat-test 2465)
(fermat-test 2821)
(fermat-test 6601)

(define (timed-prime-test n) 
	
	(define (start-prime-test n) 
		(if (fermat-test n) 
			(report-prime (- 10000 (set-timer 0))) ;(set-timer 0) return elapsed procedures
			#f)) 
	(define (report-prime elapsed-time) 
		(display " *** ") 
		(display elapsed-time)) 
	
	(newline) 
	(display n) 
	(set-timer 10000) ; timer start
	(start-prime-test n))
  
 (timed-prime-test 1999)