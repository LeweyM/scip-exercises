#lang sicp

(#%require rackunit)

;2.33

(define (accumulate proc initial l)
  (if (null? l)
      initial
      (proc (car l)
            (accumulate proc initial (cdr l)))))

(define (square x) (* x x))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(check-equal? (accumulate + 0 '(1 2 3 4)) 10)
(check-equal? (map square '(1 2 3 4)) '(1 4 9 16))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(check-equal? (append '(1 2) '(3 4)) '(1 2 3 4))

(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

(check-equal? (length '(1 2 3 4 5)) 5)

;2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)) )
              0
              coefficient-sequence))

(check-equal? (horner-eval 2 '(1 3 0 5 0 1)) 79)

;2.35

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (el) (if (pair? el) (count-leaves el) 1)) t)))

(check-equal? (count-leaves '(0 (1 (2 3 2 (4))))) 6)

;2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (s) (car s)) seqs))
            (accumulate-n op init (map (lambda (s) (cdr s)) seqs)))))

(check-equal? (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) '(22 26 30))

