#lang racket
; half interval

(define (close-enough? f g)
  (< (abs (- f g)) 0.001)
  )

(define (pos? n)
  (> n 0)
  )

(define (neg? n)
  (< n 0)
  )

(define (average x y)
  (/ (+ x y) 2)
  )

(define (half-interval-safe f a b)
  (let (
        (a-val (f a))
        (b-val (f b))
        )
    (cond
      ((and (neg? a-val) (pos? b-val))
       (half-interval f a b))
      ((and (pos? a-val) (neg? b-val))
       (half-interval f b a))
      (else
       (error "not the right signs"))
      )
    )
  )

(define (half-interval func a b)
  (let ((mid-point (average a b)))
    (if (close-enough? a b)
        mid-point
        (let ( (test (func mid-point)) )
          (cond
            ((close-enough? a b) mid-point)
            ((pos? test)
             (half-interval func a mid-point))
            ((neg? test)
             (half-interval func mid-point b))
            (else mid-point)
            )
          )
        )
    )
  )

;pi
(half-interval-safe sin 2.0 4.0)

; x^3 - 2x - 3 == 0 ; 1.893066...
(half-interval-safe (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0 )



