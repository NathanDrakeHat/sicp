(define (my-squeeze items) ; leaves to list from left to right
  (define (iter items res) 
    (cond [(null? items) res] ; end case
          [(not (pair? (car items))) (iter (cdr items) (cons (car items) res)) ] ; list case
          [else (iter (cdr items) (append (iter (car items) '()) res)) ]
    )
  ) ; leaf case
  (reverse [iter items '()] )
)
(define (squeeze tree) ; same as squeeze
  (cond [(null? tree) '()] 
        [(not (pair? tree)) (list tree)] 
        [else (append (squeeze (car tree)) (squeeze (cdr tree)))]
  )
)

(define a (list (list 1 2) (list 4 5)))
a
(squeeze a)

(define t (list 0 a (list a 1) 2)) ; list has end
t
(squeeze t)