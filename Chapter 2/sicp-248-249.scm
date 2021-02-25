(define (make-vect x y)
  (cons x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
	     (* (ycor-vect v) s)))

(define (equals-vect? v1 v2)
  (and (= (xcor-vect v1) (xcor-vect v2))
       (= (ycor-vect v1) (ycor-vect v2))))

					; 2.48

(define (make-segment start end) (cons start end))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

					; 2.49

(define (bottom-left) (make-vect 0 0))
(define (top-left) (make-vect 0 1))
(define (bottom-right) (make-vect 1 0))
(define (top-right) (make-vect 1 1))

(define (outline-painter f)
  (list
   (make-segment top-left top-right)
   (make-segment top-right bottom-right)
   (make-segment bottom-right bottom-left)
   (make-segment bottom-left top-left)))

(define (cross-painter f)
  (list
   (make-segment top-left bottom-right)
   (make-segment top-right bottom-left)))


(define (mid-left) (make-vect 0 0.5))
(define (mid-top) (make-vect 0.5 1))
(define (mid-right) (make-vect 1 0.5))
(define (mid-bottom) (make-vect 0.5 0))


(define (diamond-painter f)
  (list
   (make-segment mid-left mid-top)
   (make-segment mid-top mid-right)
   (make-segment mid-right mid-bottom)
   (make-segment mid-bottom mid-left)))

					;tests

(define (t-1)
  (let ((s (make-vect 0 0))
	(e (make-vect 1 1)))
    (let ((seg (make-segment s e)))
      (begin
	(assert (equals-vect? (start-segment seg) s))
	(assert (equals-vect? (end-segment seg) e))
	))))

(define (test)
  (begin
    (t-1)
   "All tests passed"))

(test)
