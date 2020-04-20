#lang sicp

(#%require rackunit)

(define (accumulate proc initial l)
  (if (null? l)
      initial
      (proc (car l)
            (accumulate proc initial (cdr l)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (s) (car s)) seqs))
            (accumulate-n op init (map (lambda (s) (cdr s)) seqs)))))

(check-equal? (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) '(22 26 30))


;2.37

(define (matrix-*-vector m v)
  (map (lambda (row) (accumulate + 0 (map * row v))) m))

(check-equal? (matrix-*-vector '((1 -1 2) (0 -3 1)) '(2 1 0)) '(1 -3))

(define (transpose m)
  (accumulate-n cons '() m))

(check-equal? (transpose '((1 2) (3 4) (5 6))) '((1 3 5) (2 4 6)))

(define (matrix-*-matrix m n) 
   (let ((n-cols (transpose n))) 
     (map (lambda (m-row) (matrix-*-vector n-cols m-row)) m))) 

(check-equal? (matrix-*-matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))
                               (list (list 1 2) (list 1 2) (list 1 2) (list 1 2)))
              '((10 20) (26 52) (42 84)))

;2.38

(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))


; operations must be commutative (order independant) for fold-left and fold-right to have equal effect.

;2.39

(define (reverse-r l)
  (fold-right (lambda (x acc) (append acc (list x))) '() l))

(check-equal? (reverse-r (list 1 2 3 4 5)) (list 5 4 3 2 1))

(define (reverse-l l)
  (fold-left (lambda (x acc) (cons acc x)) '() l))

(check-equal? (reverse-l (list 1 2 3 4 5)) (list 5 4 3 2 1))

;2.40

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence)))))

 (define (prime? x) 
   (define (test divisor) 
     (cond ((> (* divisor divisor) x) true) 
           ((= 0 (remainder x divisor)) false) 
           (else (test (+ divisor 1))))) 
   (test 2)) 

(define (enumerate-interval low high) 
   (if (> low high) 
       nil 
       (cons low (enumerate-interval (+ low 1) high)))) 

(define (unique-pairs n)
  (let ((enums (enumerate-interval 1 n)))
    (flatmap (lambda (i)
           (map (lambda (j) (list i j))
                (enumerate-interval (+ i 1) n))) enums)))

(check-equal? (unique-pairs 4) '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))

(define (prime-sum-pairs n)
  (map (lambda (p) (list (car p) (cadr p) (+ (car p) (cadr p))))
       (filter (lambda (pair) (prime? (+ (car pair) (cadr pair))))
               (unique-pairs n))))

(check-equal? (prime-sum-pairs 4) '((1 2 3) (1 4 5) (2 3 5) (3 4 7)))

;2.41

(define (sums-to? n)
  (lambda (triplet)
    (= n (+ (car triplet) (cadr triplet) (caddr triplet)))))

(define (enumerate-unique-triplets n)
  (flatmap (lambda (i)
         (flatmap (lambda(j)
                (map (lambda (k) (list i j k))
                     (enumerate-interval (+ j 1) n)))
              (enumerate-interval (+ i 1) n)))
       (enumerate-interval 1 n)))

(define (sum-triplets n)
  (filter (sums-to? n) (enumerate-unique-triplets n)))

(check-equal? (enumerate-unique-triplets 4) '((1 2 3) (1 2 4) (1 3 4) (2 3 4)))

(check-equal? (sum-triplets 6) '((1 2 3)))
(check-equal? (sum-triplets 8) '((1 2 5) (1 3 4)))



