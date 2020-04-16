#lang racket

(require rackunit)

;2.17

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(check-equal? (last-pair (list 1 2 3 34)) (list 34))

;2.18

(define (reverse l)
  (if (null? (cdr l))
      l
      (append (reverse (cdr l))
              (list (car l)))))

(check-equal? (reverse (list 1 2 3 34)) (list 34 3 2 1))

;2.19

(define (no-more? l)
  (null? l))

(define (except-first-denomination l)
  (cdr l))

(define (first-denomination l)
  (car l))

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(check-equal? (cc 100 us-coins) 292)

;  the order of the coins does not affect the outcome as we call except-first-denomination - i.e. all the other coins - with the full amount
;  being computed. This ensures that all combinations are tested regardless of order.

;2.20

(define (same-parity . n)
  (same-parity-recur n 't))

(define (same-parity-recur l include?)
  (cond
    ((null? l) '() )
    (include? (cons
               (car l)
               (same-parity-recur (cdr l) (not include?))))
    (else (same-parity-recur (cdr l) (not include?)))))

(check-equal? (same-parity 1 2 3 4 5) '(1 3 5))
(check-equal? (same-parity 2 3 4 5) '(2 4))



