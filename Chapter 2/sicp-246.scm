					;2.46

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

					;tests

(define (t-vect)
  (let ((v1 (make-vect 1 2)))
    (begin
      (assert (= 1 (xcor-vect v1)))
      (assert (= 2 (ycor-vect v1))))))

(define (t-add)
  (let ((sum (add-vect (make-vect 1 2) (make-vect 2 4))))
    (begin
      (assert (= 3 (xcor-vect sum)))
      (assert (= 6 (ycor-vect sum))))))

(define (t-sub)
  (let ((res (sub-vect (make-vect 5 5) (make-vect 2 4))))
    (begin
      (assert (= 3 (xcor-vect res)))
      (assert (= 1 (ycor-vect res))))))

(define (t-scale)
  (let ((res (scale-vect (make-vect 5 6) 2)))
    (begin
      (assert (= 10 (xcor-vect res)))
      (assert (= 12 (ycor-vect res))))))

(define (test)
  (begin
    (t-vect)
    (t-add)
    (t-sub)
    (t-scale)
    "All tests passed"))

(test)


