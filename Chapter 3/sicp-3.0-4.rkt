; 3.1

(define (make-accumulator initial) 
  (let ((sum initial))
    (lambda (x) (begin (set! sum (+ sum x))
                       sum))))

(define A (make-accumulator 5))
(A 10) ; 15
(A 10) ; 25

; 3.2

(define (make-monitored fn)
  (let ((counter 0)) 
    (lambda (x) 
      (cond 
        [(equal? x 'how-many-calls?) counter]
        [(equal? x 'reset-count) (set! counter 0)]
        [else (begin (set! counter (+ 1 counter))
                     (fn x))]))))

(define s (make-monitored sqrt))

(s 100) ; 10
(s 'how-many-calls?) ; 1
(s 81) ; 9
(s 9) ; 3
(s 'how-many-calls?) ; 3
(s 'reset-count) 
(s 'how-many-calls?) ; 0

; 3.3

(define (make-account balance secret)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (if (equal? password secret)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m)))
      (lambda (x) (error "Incorrect Password"))))
  dispatch)

(define acc (make-account 100 'secret))

((acc 'secret 'withdraw) 40) ; 60
((acc 'bad-password 'deposit) 50) ; incorrect password

; 3.4

(define (call-the-cops) (error "woop woop - sound of da police"))

(define (make-account balance secret)
  (let ((attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch password m)
      (if (equal? password secret)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x) (begin (set! attempts (+ 1 attempts))
                           (if (> attempts 3)
                             (call-the-cops)
                             (error "Incorrect Password"))))))
    dispatch))


(define acc (make-account 100 'secret))

((acc 'secret 'withdraw) 40) ; 60
((acc 'bad-password 'deposit) 50) ; incorrect password
((acc 'bad-password 'deposit) 50) ; incorrect password
((acc 'bad-password 'deposit) 50) ; incorrect password
((acc 'bad-password 'deposit) 50) ; woop woop - sound of da police
