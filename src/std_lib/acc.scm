(define (seq-acc op initial sequence)
  (if [null? sequence]
      initial
      [op (car sequence) (seq-acc op initial (cdr sequence))]
	)
) ; operate current value and next seq-acc

;example below
(define (horner-eval x coefficient-sequence)
  (seq-acc (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms))) 0 coefficient-sequence)
)
(horner-eval 2 (list 1 3 0 5 0 1))


(define (count-leaves items) 
	(seq-acc [lambda (a b) (if (pair? a) 
																(+ (count-leaves a) b) 
																(+ 1 b))] 
							0 items)
)
(count-leaves (list 1 (list 2 3) 4 (list 5 (list 6))))

(define (corres-acc op init seqs)
  (if [null? (car seqs)]
      '()
      [cons (seq-acc op init (map car seqs)) (corres-acc op init (map cdr seqs))]
	)
)
(corres-acc + 0 (list (list 1 0) (list 1 0) (list 1 0)))