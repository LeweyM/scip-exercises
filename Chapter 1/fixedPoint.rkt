#lang racket
;1.3.3

(define (close-enough? f g)
  (< (abs (- f g)) 0.000001)
  )
  

(define (fixed-point func guess)
  (let ((next (func guess)))
    (cond
      ((close-enough? next guess) next)
      (else (fixed-point func next))
          )
    )
  )


(displayln (fixed-point (lambda (y) (+ (sin y) (cos y))) 1 ) ) ;1.2587...


; 1.3.4

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqr x) (* x x))

(define (cube x) (* x x x))

((average-damp sqr) 10) ;55


(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(sqrt 2) ;1.41421356237

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (sqr y))))
               1.0))

(cube-root 9) ;2.08008382305

;newtons method with fixed-point

(define (deriv g)
  (let (
        (d 0.00001))
    (lambda (x) (/ (- (g (+ x d)) (g x))
                   d))))

((deriv cube) 5) ;75.0001499

(define (newtons-method g)
  (fixed-point (lambda (x)
                 (- x (/ (g x) ((deriv g) x))))
               1.0))

(define (newton-sqrt x)
  (newtons-method (lambda (y) (- (sqr y) x))))

(newton-sqrt 2) ;1.41421356237

;exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (sqr x)) (* b x) c)))

(newtons-method (cubic 2 3 4)) ;x1 = -1.65063 ;x2 = -0.17469 + i * 1.54687 ;x3 = -0.17469 - i * 1.54687

;exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5) ;21

;exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose sqr inc) 6) ;49

;exercise 1.43
(define (repeated f k)
  (if (= k 1)
      f
      (compose f (repeated f (- k 1)))))

((repeated sqr 2) 5) ;625

;exercise 1.44
(define dx 0.0001)

(define (average-3 a b c)
  (/ (+ a b c) 3))

(define (smooth f)
  (lambda (x) (average-3 (- x dx) x (+ x dx))))

(define (smooth-fold f n)
  (repeated smooth n) f)

;exercise 1.45

(define (power-of x n)
  (if (= n 0)
      1
      (* x (power-of x (- n 1)))))

(power-of 3 0)
(power-of 3 1)
(power-of 3 2)
(power-of 3 3)
(power-of 3 4)
(power-of 3 5)

(define (n-root x n k)
  (fixed-point ((repeated average-damp n) (lambda (y) (/ x (power-of y k))))
               1.0) )

(n-root 9 1 2)
;(n-root 27 1 3)
(n-root 81 2 3)
(n-root 243 2 4)
(n-root 729 2 5)
(n-root 2187 2 6)







