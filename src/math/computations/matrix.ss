(define (list-fold-right op init seqs)
  (if [null? (car seqs)]
      '()
      [cons (fold-right op init (map car seqs)) (list-fold-right op init (map cdr seqs))]
	)
)

(define (dot-product v w)
  (map [lambda (row1 row2) (map * row1 row2)] v w)
)
(define (vector-product m v)
  (map [lambda (row) (fold-right + 0 (map * row (map car v)))] m)
)

(define (transpose mat)
  (define concatenate 
    [lambda (mat11 mat12) 
      (cons mat11 mat12)
    ]
  )
  (list-fold-right concatenate '() mat) ; mat11 is M(1, 1), and so on
)

(define (matrix-product m n) 
  (let ((cols (transpose n))) 
		(define row-product-cols 
			(lambda [row] 
				[map (lambda (col) (fold-right + 0 (map * row col))) cols]
			)
		) 
    (map row-product-cols m) 
	)
)

(define a (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define b (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define v (list (list 1) (list 1) (list 1)))
(define two (list (list 2 0 0) (list 0 2 0) (list 0 0 2)))
(dot-product a b)
(vector-product b v)
(transpose b)
(matrix-product b two)
