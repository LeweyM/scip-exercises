#lang racket

(require rackunit)

(define (square x) (* x x))

(check-equals? (square 5) 25)

(square 20)