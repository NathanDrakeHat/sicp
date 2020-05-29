(define (enumerate-interval low high)
	(if [> low high] 
			'() 
			[cons low (enumerate-interval (+ low 1) high)]
	)	
)

(define (flat-map proc seq) ; less to many map
  (fold-right append '() (map proc seq))
)

(define (ordered-triples n s)
	(define to-pair
		(lambda (i) (map [lambda (j) (list j i)] (enumerate-interval 2 (- i 1))))
	)
	(define to-triple 
		(lambda (pair) 
			(map (lambda (k) (cons k pair)) (enumerate-interval 1 (- (car pair) 1)))
		)
	)
	(define (make-triple-sum s)
		(lambda (triple)
			(append triple (list s))
		)
	)
	(define (eq-sum s) 
		(lambda (triple)
			(= s (fold-right + 0 triple))
		)
	)
	(map (make-triple-sum s) (filter (eq-sum s) 
			[flat-map to-triple [flat-map to-pair (enumerate-interval 3 n)]]))
)
;find ordered-triple (k, j, i) 1<= k < j < i<=n and k+j+i = s
(ordered-triples 8 13)

