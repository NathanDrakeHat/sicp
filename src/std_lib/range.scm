(define (range low high)
	(if [> low high] 
			'() 
			[cons low (range (+ low 1) high)]
	)	
)

(range 0 4)