(define (make-accumulator a)
  (define (acc i)
    (begin (set! a (+ a i)) a)
  )
  acc
)
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"
    )
  )
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance
  )
  (define (call-cops num)
    "we have called police!"
  )
  (define (op-error m)
    (lambda (n) m)
  )
  (define (dispatch m p)
    (if (equal? p password)
      (begin (set! wrong 0)
        (cond [(equal? m 'withdraw) withdraw]
              [(equal? m 'deposit) deposit]
              [else (op-error "Unknown request: MAKE-ACCOUNT")]
        )
      )
      (if (< wrong 5)
          (begin (set! wrong (+ wrong 1)) (op-error "wrong password"))
          call-cops
      )
    )
  )
  (let [(wrong 0)]
    dispatch
  )
)
(define make-monitored-func
  (let ([count 0])
    (lambda (func) 
      (lambda (first . rest)
        (cond [(equal? first "how many calls?") count]
              [(equal? first "reset count") (begin (set! count 0) count)]
              [else (begin (set! count (+ count 1)) (apply func (cons first rest)))]      
        )
      )
    )
  )
)
(define (message-passing m)
  (define add
    (lambda (f . r)
      (apply + (cons f r))
    )
  )
  (define minus
    (lambda (f . r)
      (apply - (cons f r))
    )
  )
  (define (exception m)
    (lambda (f . r) m)
  )
  (cond [(equal? m "add") add]
        [(equal? m "minus") minus]
        [else (exception "not have this operator--PROC:message-passing.")]
  )
)
((message-passing "add") 1 2 3)
((message-passing "minus") 1 2 3)
((message-passing "sqrt") 4)
