(define (simpson-integral func a b n) ;h/3 * (y0 + 4y1 + 2y2 + 4y3 + ... + 2yn-2 + 4yn-1 + yn)
	(define (integral-term k)         ;h = (b - a)/n, yk = f(a+k*h)
		(cond [(or (= k n) (= k 0)) (func (+ a (* k (/ (- b a) n))))] ;
			[(even? k) (* 2 [func (+ a (* k (/ (- b a) n)))])] ;
			[else (* 4 [func (+ a (* k (/ (- b a) n)))])])) 
	(define (sigma k)
		(if (> k n) 0 (+ (integral-term k) [sigma (+ k 1)]))) 
	(let [(h  (/ (- b a) (if (= (remainder n 2) 0) n (+ n 1))))] ; n should be even 
		(* (/ h 3) (sigma 0))))
	
(simpson-integral (lambda (v) (* v v v)) 0 1 1000)
(simpson-integral (lambda (v) (* v v)) 0 1 1000)


;normal integral definition
(define (integral func a b dx)  
	(define (sigma l u) 
		(if (> l u) 
			0 
			(+ (func l) (sigma ([lambda (v) (+ v dx)) l] u)))) 
	(* (sigma [+ a (/ dx 2)] b) dx))
	
;(integral (lambda (x) (* x x x)) 0 1 0.0001)