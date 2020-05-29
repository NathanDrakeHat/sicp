(define (leaf-map f items) ; leaves to list from left to right
  (define (iter items res) 
    (cond [(null? items)  res] ; end case
          [(not  (pair? (car items))) (iter [cdr items] [cons (f (car items)) res]) ] ; list case
          [else  (iter [cdr items] [cons (reverse (iter (car items) '())) res]) ] ; leaf case
    )
	)
  (reverse [iter items '()])
)

(define a (list (list 1 2) (list 4 5)))
a
(leaf-map (lambda (v) (* v v)) a)

(define t (list 0 a (list a 1) 2)) ; list has end
t
(leaf-map (lambda (v) (* v v)) t)