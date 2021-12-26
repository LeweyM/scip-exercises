; 3.7

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
      (lambda (x)  (error "Incorrect Password"))))
  dispatch)

(define (make-joint acc oldpass newpass)
   (define (dispatch password m)
    (if (equal? password newpass)
      (cond ((eq? m 'withdraw) (acc oldpass 'withdraw))
            ((eq? m 'deposit) (acc oldpass 'deposit))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m)))
      (lambda (x)  (error "Incorrect Password"))))  
   dispatch)

(define foo-account (make-account 50 'foo))
(define bar-account (make-joint foo-account 'foo 'bar))
((foo-account 'foo 'withdraw) 10) ;40
((bar-account 'bar 'withdraw) 10) ;30

; the issue with this implementation is that we need to add links for each method in make-joint and make-account. It would be better if we could abstract the password application so that the passwords of each dispatch method could be handled implicitly.

(define (with-password password target)
  (lambda (password-attempt m)
    (if (equal? password-attempt password) (target m)
      (lambda (x) (error "Incorrect Password")))))

(define (make-account balance secret)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  (with-password secret dispatch))

(define (make-joint acc oldpass newpass)
  (with-password newpass (lambda (m) (acc oldpass m))))

(define foo-account (make-account 50 'foo))
(define bar-account (make-joint foo-account 'foo 'bar))
((foo-account 'foo 'withdraw) 10) ;40
((bar-account 'bar 'withdraw) 10) ;30

; 3.8

(define (f x)
  (begin (set! val (+ val x)) val))

(define val 0)
(+ (f 0) (f 1))
(define val 0)
(+ (f 1) (f 0))
