#lang racket

(require rackunit)

;1.2.2

;pg40 - example

; count the different ways you can make change from one input


(define (changer x)
  (make-change x 5))

(define (make-change x coin-n)
  (cond
    ((= x 0) 1)
    ((or (< x 0) (= coin-n 0)) 0)
    (else (+
           (make-change x (- coin-n 1))
           (make-change (- x (get-coin coin-n)) coin-n)))))

(define (get-coin n)
  (cond
    ((= n 1) 1)
    ((= n 2) 5)
    ((= n 3) 10)
    ((= n 4) 25)
    ((= n 5) 50)))

(check-equal? (changer 100) 292) 
(check-equal? (changer 10) 4) 