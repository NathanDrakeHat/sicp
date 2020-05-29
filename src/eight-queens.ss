(define (list-map proc seq)
  (fold-right append '() (map proc seq))
)

(define (enumerate-interval low high) 
	(if [> low high] 
			'() 
			[cons low (enumerate-interval (+ low 1) high)]
	)	
)

(define (queens board-size)
  ; result is a list of matrixs
  ; MATRIX ROW MEANS CHESS BOARD COLUMN, ON CONTRAY TO CONVENTION!

  (define empty-board ; all zero matrix
    (map [lambda (p) 
          (map (lambda (q) 0) (enumerate-interval 1 board-size))] 
    [enumerate-interval 1 board-size])
  )
  (define (adjoin-position new-row k rest-of-queens) ; Matrix (k, new-row) <-- 1
    (map  [lambda (col) 
            (if [= col k]
                (map  [lambda (val) 
                      (if (= val new-row) 1 0)
                      ] 
                  (enumerate-interval 1 board-size)
                )
                (list-ref rest-of-queens (- col 1))
            )
          ] 
    (enumerate-interval 1 board-size)) 
  )
  (define (row-index lst) ; [0 0 1 0] -> 3
    (define (iter i)
      (if (= 1 (list-ref lst (- i 1)))
        i
        (iter (+ i 1))
      )
    )
    (iter 1)
  )
  (define (safe? k positions) ; no two queens one the same line 
    (let [(row-idx (row-index (list-ref positions (- k 1))))]
      (null?  (filter [lambda (col) 
                        (let [(r (row-index (list-ref positions (- col 1))))]
                          (if (and [not (= r row-idx)] 
                                    [not (= (+ r col) (+ row-idx k))] 
                                    [not (= (- r col) (- row-idx k))]
                              )
                              (= 1 2) ; filter out safe col
                              (= 1 1)
                          )
                        )
                      ]   
              (enumerate-interval 1 (- k 1)))) ; null if all columns is safe
    )
  )
  (define (queen-cols k) ; all posible result after push one queen on k'th column
    (if [= k 0]
        (list empty-board)
        (filter
          [lambda (positions) 
            (safe? k positions)
          ]
        (list-map ; one board to eight board
          [lambda (rest-of-queens) ; input one board, return eight result board
            (map  [lambda (new-row) 
                    (adjoin-position new-row k rest-of-queens)
                  ]
            (enumerate-interval 1 board-size))
          ]
        (queen-cols (- k 1))))
    )
  )
  (queen-cols board-size)
)

(display "Number of solutions:\n")
(/ (length (queens 8)) 4)