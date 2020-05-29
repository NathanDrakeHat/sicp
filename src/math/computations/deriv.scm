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
  (and (pair? x) (eq? (car x) '+))
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
  (cadr s)
)
(define (continue-sum lst) ; (1 2 3) --> (+ 1 (+ 2 3))
  (if [= (length lst) 1] 
      [car lst] 
      [make-sum (car lst) (continue-sum (cdr lst))])
)
(define (augend s) 
  (let [(t (cddr s))] 
      [if (= (length t) 1) 
          (car t) 
          (list '+ (car t) (continue-sum (cdr t)))]
  )
)
(define (product? x)
  (and (pair? x) (eq? (car x) '*))
)
(define (multiplier p) 
  (cadr p)
)
(define (continue-product lst)
  (if [= (length lst) 1] 
      [car lst] 
      [make-product (car lst) (continue-product (cdr lst))])
)
(define (multiplicand p) 
  (let [(t (cddr p))] 
      [if (= (length t) 1) 
          (car t) 
          (list '* (car t) (continue-product (cdr t)))]
  )
)
(define (make-sum a1 a2) 
  (cond [(=number? a1 0) a2] 
        [(=number? a2 0) a1] 
        [(and (number? a1) (number? a2)) (+ a1 a2)] 
        [else (list '+ a1 a2)]
  )
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

(define (deriv expr var)
  (cond [(number? expr) 0]
        [(variable? expr) (if (same-variable? expr var) 1 0)]
        [(sum? expr)
          (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var))]
        [(product? expr)
          (make-sum
            (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
            (make-product (deriv (multiplier expr) var)
                         (multiplicand expr)))] 
        [(exponentiaion? expr) 
          (make-product (exponent expr) 
            (make-product [make-exponentiaion (base expr) (- (exponent expr) 1)] [deriv (base expr) var]))]
        [else
          (error "unknown expression type -- DERIV" expr)]
  )
)

(deriv '(+ x 2) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(** x 2) 'x)
(deriv '(* x y (+ x 3)) 'x) 