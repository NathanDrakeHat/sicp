(define tolerance 0.0000000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (newton-method g guess) ; get x for g(x) = 0
	(define (deriv g) 
		(lambda (x) 
			(let [(dx 0.0000001)] 
				[/ (- [g (+ x dx)] (g x)) dx]))) 
	(define (newton-transform g) ; x - g(x)/g'(x)
		(lambda (x) 
			[- x (/ [g x] [(deriv g) x])]))
	(fixed-point (newton-transform g) guess))
	
(define (newton-method-sqrt x) 
	(newton-method (lambda (y) (- (* y y) x)) 1.0))
	
(newton-method-sqrt 100)