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
  
(fixed-point cos 1.0)

(define (fixed-point-sqrt x)
  (fixed-point (lambda (y) ((lambda (a b) (/ (+ a b) 2)) y (/ x y)))
               1.0))
			   
(fixed-point-sqrt 0.0009)

(define phi ; x -> 1+1/x
	(fixed-point (lambda (v) ([lambda (a b) (/ (+ a b) 2)] v (+ 1 (/ 1 v)))) 1.6))
phi