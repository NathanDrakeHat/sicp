(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right)
)
(define (element-of-set? x set)
  (cond [(null? set) false]
        [(= x (entry set)) true]
        [(< x (entry set)) (element-of-set? x (left-branch set))]
        [(> x (entry set)) (element-of-set? x (right-branch set))]
  )
)
(define (adjoin-set x set)
  (cond [(null? set) (make-tree x '() '())]
        [(= x (entry set)) set]
        [(< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set))]
        [(> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set)))]
  )
)
(define (tree->list tree)
  (if (null? tree)
    '()
    (append [tree->list (left-branch tree)] (cons (entry tree) [tree->list (right-branch tree)])
    )
  )
)

(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let [(left-size (quotient (- n 1) 2))]
            (let [(left-result (partial-tree elts left-size))]
                  (let [(left-tree (car left-result)) 
                        (non-left-elts (cdr left-result)) 
                        (right-size (- n (+ left-size 1)))
                        ]
                        (let [(this-entry (car non-left-elts))
                              (right-result (partial-tree (cdr non-left-elts) right-size))
                              ]
                              (let [(right-tree (car right-result))
                                    (remaining-elts (cdr right-result))
                                    ]
                                    (cons (make-tree this-entry left-tree right-tree) remaining-elts)
                              )
                        )
                  )
            )
        )
    )
  )
  (car (partial-tree elements (length elements)))
)
(define (union-set s1 s2)
  (define (union-list lst1 lst2 res)
    (cond [(null? lst1) (append res lst2)]
          [(null? lst2) (append res lst1)]
          [else (let [(x1 (car lst1)) (x2 (car lst2))]
                  (cond [(> x1 x2) (union-list lst1 (cdr lst2) (append res (list x2)))]
                        [(< x1 x2) (union-list (cdr lst1) lst2 (append res (list x1)))]
                        [else (union-list (cdr lst1) (cdr lst2) (append res (list x1)))]
                  )
                )]
    )
  )
  (let [(l1 (tree->list s1)) (l2 (tree->list s2))]
    (list->tree (union-list l1 l2 '()))
  )
)
(define (intersection-set s1 s2)
  (define (intersection-list lst1 lst2 res)
    (if (or (null? lst2) (null? lst1)) 
        res
        (let [(x1 (car lst1)) (x2 (car lst2))]
            (cond [(> x1 x2) (intersection-list lst1 (cdr lst2) res)]
                  [(< x1 x2) (intersection-list (cdr lst1) lst2 res)]
                  [else (intersection-list (cdr lst1) (cdr lst2) (append res (list x1)))]
            )
        )
    )
  )
  (let [(l1 (tree->list s1)) (l2 (tree->list s2))]
    (list->tree (intersection-list l1 l2 '()))
  )
)
(define a (list->tree (list 1 2 3 4 5 6 7)))
(define b (list->tree (list 5 6 7 8 9 10)))
(union-set a b)
(intersection-set a b)