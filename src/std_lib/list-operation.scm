(define (flat-map proc seq) ; less to many map
  (fold-right append '() (map proc seq))
)
; one to many map
; (1, 2, 3) --> ((1, 2), (2, 3) (3, 4)) --> (1, 2, 2, 3, 3, 4)
; or say list element map

(define (list-fold-right op init seqs)
  (if [null? (car seqs)]
      '()
      [cons (fold-right op init (map car seqs)) (list-fold-right op init (map cdr seqs))]
	)
)
; (list-fold-right op init (list lst1 lst2)) --> (list (fold-right op init lst1) (fold-right op init lst2)) 