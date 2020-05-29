(define (smallest-divisor n) ; old version 
	(define (find-divisor n test-divisor)
	  (cond [(> ((lambda (v) (* v v)) test-divisor) n) n]
			[(divides? test-divisor n) test-divisor]
			[else (find-divisor n (+ test-divisor 1))])) 
	(define (divides? a b) 
		(= (remainder b a) 0)) 
	
	(set-timer 10000) 
	(display (find-divisor n 2)) 
	(newline) 
	(display "time elapsed: ") 
	(display (- 10000 (set-timer 0))) 
	(newline))

(define (smallest-divisor-with-increase n) ; faster version
	(define (next n) ; approximately twice faster
		(cond ((= n 2) 3) 
			(else (+ n 2))))
		(* x x)) 
	(define (find-divisor n test-divisor) 
		(cond [(> ((lambda (v) (* v v)) test-divisor) n) n] 
			[(divides? test-divisor n) test-divisor] 
			[else (find-divisor n (next test-divisor))])) 
	(define (divides? a b) 
		(= (remainder b a) 0)) 
	(set-timer 10000) 
	(display (find-divisor n 2)) 
	(newline) 
	(display "time elapsed: ") 
	(display (- 10000 (set-timer 0))) 
	(newline))

(smallest-divisor-with-increase 1105)
(smallest-divisor-with-increase 1999)
(smallest-divisor 1999)