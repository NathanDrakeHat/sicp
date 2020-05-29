(define (miller-rabin-test n) 
	(define (square x) (* x x)) 
	(define (neq a b) (not (= a b))) 
	(define (check root m)
		(define square-mod [remainder (square root) m])
		(if (and [= square-mod 1] [neq root 1] [neq root (- m 1)]);not-trival square root condition
			0 
			square-mod)) ;(a^p % m)^2 % m == a^2p % m
	(define (expmod a p m) ; return a^p % m
		(cond [(= p 0) 1]
			[(even? p) (check [expmod a (/ p 2) m] m)] 
			 [else (remainder [* a (expmod a (- p 1) m)] m)]))
	
	(let [(rand [if (not (= n 1)) 
					(+ 1 (random (- n 1))) 
					(- 1)])] 
		(let ((mod [expmod rand (- n 1) n])) 
			(or (= mod 1) (= rand (- 1))))))

(miller-rabin-test 1999)
(display "\nbelow should be #f:\n")
(miller-rabin-test 19999)
(miller-rabin-test 561) ;fermat test can be fooled while miller rabin test not.
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)