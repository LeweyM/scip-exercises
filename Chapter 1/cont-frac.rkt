#lang racket

; recursive
(define (cont-frac n d k i op)
  (if (= k 0)
      0
      (/
       (n i)
       (op (d i) (cont-frac n d (- k 1) (+ i 1) op)))
      )
  )
(define (cont-frac-rec n d k)
  (cont-frac n d k 0 +))


; iter
(define (cont-frac-int n d k)
  (define (cont-frac-i n d k acc)
  (if (< k 0)
      acc
      (let ((d-val (d k))
            (n-val (n k)))
        (cont-frac-i n d (- k 1) (/ n-val (+ d-val acc))))))
  (cont-frac-i n d k 0))


(cont-frac-int (lambda (i) 1.0) (lambda (i) 1.0) 30); 0.61803398875

;e
(define (series i)
  (if (= 0 (modulo (+ i 2) 3))
      (* (/ (+ i 2) 3) 2)
      1))
(+ (cont-frac-int (lambda (i) 1.0) series 30) 2); 2.71828
(+ (cont-frac-rec (lambda (i) 1.0) series 30) 2); 2.71828

;tan(x)
(define (square x)
  (lambda (i)
    (if (= i 0)
      x
      (* x x))))
(define (odd-series i) (- (* (+ i 1) 2) 1))

(define (tan-cf x k)
  (cont-frac (square x) odd-series k 0 -))

(tan-cf 1.0 50) ;1.55740772465
