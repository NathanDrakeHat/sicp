(define union-set ; s1 ∪ s2 , ordered set
  (lambda (s1 s2)
    (cond [(and (null? s1) (null? s2)) '()] 
          [(null? s1) s2] 
          [(null? s2) s1]
          [else 
            (let ([x1 (car s1)] [x2 (car s2)])
              (cond [(= x1 x2) (cons x1 (union-set (cdr s1) (cdr s2)))]
                    [(> x1 x2) (cons x2 (union-set s1 (cdr s2)))]
                    [else (cons x1 (union-set (cdr s1) s2))]          
              )
            )
          ]
    )
  )
)
(define intersection-set ; s1 ∩ s2 , ordered set 
  (lambda (s1 s2)
    (if (or (null? s1) (null? s2))
      '()
      (let [(x1 (car s1)) (x2 (car s2))]
        [cond ([= x1 x2] [cons x1 (intersection-set (cdr s1) (cdr s2))])
              ([> x1 x2] [intersection-set s1 (cdr s2)])
              (else [intersection-set (cdr s1) s2]) 
        ]    
      )
    )
  )
)
(define element-in-set? ; ordered set
  (lambda (elem s) 
    (cond [(null? elem) (= 1 0)]
          [(or (null? s) (> elem (car s))) (= 1 0)] 
          [(= (car s) elem) (= 1 1)] 
          [else (element-in-set? elem (cdr s))]
    )    
  )
)
(define adjoin-set ; add element to set , ordered 
  (lambda (elem s)
    (if [or (element-in-set? elem s) (null? elem)] 
        s
        [adjoin-set.join elem s]
    )   
  )
)
(define adjoin-set.join ; element not in set and set is not null
  (lambda (elem s)
    (if (null? s) 
      (cons elem '())
      (let ([c (car s)]) ; c: current value
        (if [> elem c]
            [cons c (adjoin-set.join elem (cdr s))]
            [cons elem s]
        )
      )
    )
  )
)
; (union-set (list 1 2) (list 1 3))
; (union-set (list 2) (list 1 3))
; (union-set '() (list 1 2))
; (union-set '() (list 1))
; > (1 2 3)
; > (1 2 3)
; > (1 2)
; > (1)

(adjoin-set 0 (list 1))
(adjoin-set 1 (list 0))
(adjoin-set 1 (list 0 2))
(adjoin-set 2 (list 0 1))
(adjoin-set 2 (list 0 1))
(adjoin-set '() (list 1 2))
(adjoin-set '() (list 1))
; > (0 1)
; > (0 1)
; > (0 1 2)
; > (0 1 2)
; > (0 1 2)
; > (1 2)
; > (1)

; (intersection-set (list 0) (list 1 2))
; (intersection-set (list 1) (list 1 2))
; (intersection-set (list 0 1) (list 1 2))
; (intersection-set (list 0 1) (list 2 3))
; (intersection-set (list 0 1 3) (list 1 2 3))
; (intersection-set (list 0 1 2) (list 2 3 4))
; (intersection-set '() (list 0 1))
; (intersection-set '() (list 0))
; > ()
; > (1)
; > (1)
; > ()
; > (1 3)
; > (2)

; (element-in-set? 2 (list 0 1))
; (element-in-set? 1 (list 0 1))
; (element-in-set? 1 (list 1 2))
; (element-in-set? '() (list 1 2))
; > #f
; > #f
; > #t
; > #f