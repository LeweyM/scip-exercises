					;2.47

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
	  

(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame-1 f) (car f))  

(define (edge1-frame-1 f) (cadr f))

(define (edge2-frame-1 f) (caddr f))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2 f) (car f))  

(define (edge1-frame-2 f) (cadr f))

(define (edge2-frame-2 f) (cddr f))

(define (t-1)
  (let ((o (make-vect 0 0))
	(e1 (make-vect 1 1))
	(e2 (make-vect 2 3)))
    (let ((f (make-frame-1 o e1 e2)))
      (begin
	(assert (equals-vect? (origin-frame-1 f) o))
	(assert (equals-vect? (edge1-frame-1 f) e1))
	(assert (equals-vect? (edge2-frame-1 f) e2))
	))))

(define (t-2)
  (let ((o (make-vect 0 0))
	(e1 (make-vect 1 1))
	(e2 (make-vect 2 3)))
    (let ((f (make-frame-2 o e1 e2)))
      (begin
	(assert (equals-vect? (origin-frame-2 f) o))
	(assert (equals-vect? (edge1-frame-2 f) e1))
	(assert (equals-vect? (edge2-frame-2 f) e2))
	))))

(define (test)
  (begin
    (t-1)
    (t-2)
    "All tests passed"))

(test)
