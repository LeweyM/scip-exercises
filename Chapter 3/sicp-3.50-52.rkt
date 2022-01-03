; (require racket/stream)
; (require racket/promise)

;; force would normally be built into
;;  the stream implementation
(define (force delayed-object)
  (delayed-object))
(define (delay x) (lambda () x))

(define stream-car car)
(define (stream-cdr s) (force (cdr s)))
(define stream-null? null?)
(define cons-stream (lambda (a b) (cons a (delay b))))
(define the-empty-stream '())

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count)  (iter (+ count 1)  (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

;: (car (cdr (filter prime?
;:                   (enumerate-interval 10000 1000000))))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s)  (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;; stream-car and stream-cdr would normally be built into
;;  the stream implementation
;: (define (stream-car stream) (car stream))
;: (define (stream-cdr stream) (force (cdr stream)))

;: (stream-car
;:  (stream-cdr
;:   (stream-filter prime?
;:                  (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (memo-proc proc)
  (let ((already-run? false)  (result false))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? true)
               result)
        result))))

(define (show x)
    (display-line x)
      x)


; 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (stream-cons
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

; 3.51

(define x (stream-map show (stream-enumerate-interval 0 10)))
; '0 - printed as the car is evaluated in enumerate-interval and map

(stream-ref x 5)
; stream-ref calls (stream-cdr s) - stream-map calls (stream-cdr s) - stream-enumerate-interval alls (stream-cdr s) - returns to map and map evaluates (proc ...)
; 1
; 2
; 3
; 4
; 5

(stream-ref x 7)
; stream-cdr calls from 0 to 5 have been cached, so won't be re-evaluated.
; 6
; 7

; 3.52

(define sum 0)

(define (accum x)
    (set! sum (+ x sum))
      sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; sum = 1

(define y (stream-filter even? seq))
; sum = 2

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
; sum = 3

(stream-ref y 7)
; sum = 31

(display-stream z)
;
