; 3.10

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))

; replacing 'let' with (lambda (var) body) exp) gives us:

(define (make-withdraw initial-amount)
  ((lambda (balance) 
     (lambda (amount)
       (if (>= balance amount)
         (begin (set! balance (- balance amount))
                balance)
         "Insufficient funds")))
   inital-amount)))


; (define W1 (make-withdraw 100))

#|
GLOBAL:
W1: make-withdraw-lambda-2

make-withdraw: pt->GLOBAL
E1: inital-amount:100

make-withdraw-lambda-1: pt->make-withdraw
E-L1: balance: 100

make-withdraw-lambda-2: pt->make-withdraw-lambda-1
--empty--
|#

; (W1 50)

#|
W1: pt->make-withdraw-lambda-2
balance->make-withdraw-lambda-1->100
|#


