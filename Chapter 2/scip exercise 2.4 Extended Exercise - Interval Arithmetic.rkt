#lang racket

(require rackunit)

(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)))

;scip - 2.1.4 Extended Exercise

;Exercise 2.7

(define (make-interval a b) (cons a b))

(define (upper-bound i) (cdr i))

(define (lower-bound i) (car i))

;tests
(define interval-1 (make-interval 1 2))
(define interval-2 (make-interval 6.12 7.48))
(check-equal? (lower-bound interval-2) 6.12)
(check-equal? (upper-bound interval-2) 7.48)

;Exercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define sub-res (sub-interval interval-2 interval-1))
(check-equal? (lower-bound sub-res) 5.12)
(check-equal? (upper-bound sub-res) 5.48)

;Exercise 2.9

(define (width-interval i)
  (abs (- (upper-bound i)
          (lower-bound i))))

(check-equal? (width-interval interval-1) 1)

(define (width-sub a b)
  (round-off (abs (- (width-interval interval-1)
                     (width-interval interval-2)))
             3))

(check-equal? (width-sub interval-1 interval-2) 0.36)


;Exercise 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (lower-bound i)
        (upper-bound i))
     2))

(define (make-center-percent c p)
  (make-interval (- c (* (/ p 100) c))
                 (+ c (* (/ p 100) c))))

(define (percent i)
  (abs (* (/ (width i)
             100)
          (center i))))

(define interval-100 (make-center-percent 100 10))
(check-equal? (lower-bound interval-100) 90)
(check-equal? (upper-bound interval-100) 110)
(check-equal? (center interval-100) 100)
(check-equal? (percent interval-100) 10)





