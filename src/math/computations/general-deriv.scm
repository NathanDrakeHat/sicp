(define (variable? x) 
  (symbol? x)
)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2))
)
(define (=number? expr num)
  (and (number? expr) (= expr num))
)
(define (sum? x)
  (and (> (length x) 2) (pair? x) (eq? (cadr x) '+))
)
(define (base expr) 
  (cadr expr)
)
(define (exponent expr) 
  (caddr expr)
)
(define (exponentiaion? expr) 
  (and (pair? expr) (eq? (car expr) '**))
)
(define (addend s) 
  (car s)
)
(define (augend s) 
  (let [(t (cddr s))] 
      [if (= (length t) 1)
          (car t)
          t
      ]
  )
)
(define (product? x)
  (and (> (length x) 2) (pair? x) (eq? (cadr x) '*))
)
(define (multiplier p) 
  (car p)
)
(define (multiplicand p) 
  (let [(t (cddr p))] 
      [if (= (length t) 1)
          (car t)
          t
      ]
  )
)

(define (continue-sum lst) ; (1 2 3) --> (+ 1 (+ 2 3))
  (if [= (length lst) 1] 
      [car lst] 
      [make-sum (car lst) (continue-sum (cdr lst))])
)
(define (make-sum a1 a2) 
  (cond [(=number? a1 0) a2] 
        [(=number? a2 0) a1] 
        [(and (number? a1) (number? a2)) (+ a1 a2)] 
        [else (list '+ a1 a2)]
  )
)
(define (continue-product lst)
  (if [= (length lst) 1] 
      [car lst] 
      [make-product (car lst) (continue-product (cdr lst))])
)
(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0] 
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list '* m1 m2)]
  )
)
(define (make-exponentiaion a b)
  (cond [(= b 0) 1] 
        [(= b 1) a] 
        [(not (variable? a)) (expt a b)] 
        [else (list '** a b)])
)

(define general-deriv
  (lambda (expr var)
    (cond [(number? expr) 0]
          [(variable? expr) (if (same-variable? expr var) 1 0)]
          [(sum? expr)
            (make-sum (general-deriv (addend expr) var)
                    (general-deriv (augend expr) var))]
          [(product? expr)
            (make-sum
              (make-product (multiplier expr)
                          (general-deriv (multiplicand expr) var))
              (make-product (general-deriv (multiplier expr) var)
                          (multiplicand expr)))] 
          [(exponentiaion? expr) 
            (make-product (exponent expr) 
              (make-product [make-exponentiaion (base expr) (- (exponent expr) 1)] [general-deriv (base expr) var]))]
          [else
            (error "unknown expression type -- DERIV" expr)]
    )
  )
)

(general-deriv '(x + 3 * (x + y + 2)) 'x)