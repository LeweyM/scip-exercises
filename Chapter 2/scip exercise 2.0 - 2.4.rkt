#lang racket

(require rackunit)

;scip - chapter 2

;rational number data abstraction

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (rat->string x)
  (string-append (number->string (numer x))
     "/"
     (number->string (denom x))))

(define (print-rat x)
  (display "\n")
  (display (rat->string x))
  )

;data
(define one-half (make-rat 1 2))

;tests
(check-equal? (rat->string one-half) "1/2")

;add - (n1*d2 + n2*d1) / d1*d2 
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

;test
(define one-third (make-rat 1 3))
(define five-sixths (add-rat one-half one-third))
(check-equal? (rat->string five-sixths) "5/6")

;sub - (n1*d2 - n2*d1) / d1*d2 
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

;sub test
(define one-sixth (sub-rat one-half one-third))
(check-equal? (rat->string one-sixth) "1/6")

;mult - (n1*n2) / (d1*d2)
(define (mult-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

;mult test
(define one-fourth (mult-rat one-half one-half))
(check-equal? (rat->string one-fourth) "1/4")

;scip - exercise 2.1 - normalize negative values for constructor

(define (make-rat-NEW n d)
  (cond
    [(or (and (negative? n) (positive? d))
         (and (positive? n) (positive? d)))
     (cons n d)]
    [else (cons (- 0 n) (- 0 d))]))

(check-equal? (rat->string (make-rat-NEW -1 -2)) "1/2")
(check-equal? (rat->string (make-rat-NEW -1 2)) "-1/2")
(check-equal? (rat->string (make-rat-NEW 1 -2)) "-1/2")
(check-equal? (rat->string (make-rat-NEW 1 2)) "1/2")

;scip - exercise 2.2 - make a line-segment data abstraction

; make-segment
(define (make-segment p1 p2) (cons p1 p2))

; start-segment
(define (start-segment s) (car s))

; end-segment
(define (end-segment s) (cdr s))

; point
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

; point->string
(define (point->string p)
  (string-append "("
                  (number->string (x-point p))
                  ","
                  (number->string (y-point p))
                  ")"))

; test - constructor and selectors
(define origin (make-point 0 0))
(define ten-ten (make-point 10 10))
(define segment (make-segment origin ten-ten))
(check-equal? (point->string (start-segment segment)) "(0,0)")
(check-equal? (point->string (end-segment segment)) "(10,10)")

; midpoint-segment
(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
              (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))

(check-equal? (point->string (midpoint-segment segment)) "(5,5)")

; exercise 2.3 - make a rectangle data abstraction

; make-rect
(define (make-rect x y w h)
  (list x y w h))

; height
(define (height-rect r)
  (car (cdr (cdr (cdr r)))))

; width
(define (width-rect r)
  (car (cdr (cdr r))))

(define my-rect (make-rect 0 0 4 6))
(check-equal? (height-rect my-rect) 6)
(check-equal? (width-rect my-rect) 4)

; perim
(define (perim-rect r)
  (+ (* 2 (height-rect r))
     (* 2 (width-rect r))))
(check-equal? (perim-rect my-rect) 20)

; area
(define (area-rect r)
  (* (height-rect r)
     (width-rect r)))
(check-equal? (area-rect my-rect) 24)

; exercise 2.4 - alternative cons representation

; alt-cons
(define (alt-cons x y)
  (lambda (m) (m x y)))

(define (alt-car z)
  (z (lambda (p q) p)))

(define result-car (alt-car (alt-cons 5 10)))
(check-equal? result-car 5)

; substitution method
; (alt-car (lambda (m) (m 5 10)))
; ((lambda (m) (m 5 10)) (lambda (p q) p))
; ((lambda (p q) p) 5 10)

(define (alt-cdr z)
  (z (lambda (p q) q)))

(define result-cdr (alt-cdr (alt-cons 5 10)))
(check-equal? result-cdr 10)


