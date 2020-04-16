#lang racket

;iterative square root function using newtons method

(define (abs x)
  (cond
    ((< x 0) (- 0 x))
    (true x)
    )
  )

(define (sqrt-iter guess x)
  (displayln guess)
  (cond
    ((good-enough? guess x) guess)
    (else (sqrt-iter (improve-guess guess x) x))
    )
  )

(define (average x y)
  (/ (+ x y) 2)
  ) 

(define (improve-guess guess x)
  (average guess (/ x guess))
  )

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
  )

(define (sqrt x)
  (sqrt-iter 1.0 x))

(displayln (sqrt 2))