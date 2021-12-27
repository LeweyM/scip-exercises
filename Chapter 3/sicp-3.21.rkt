(define (front-ptr queue)  (car queue))
(define (rear-ptr queue)  (cdr queue))
(define (set-front-ptr! queue item)  (set-car! queue item))
(define (set-rear-ptr! queue item)  (set-cdr! queue item))

(define (empty-queue? queue)  (null? (front-ptr queue)))
(define (make-queue)  (cons '() ' ()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue))) 

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)

; 3.21 

(define (print-queue q)
  (define (helper x)
    (if (eq? x (rear-ptr q)) 
      (rear-ptr q)
      (cons (car x) (helper (cdr x)))))
  (if (empty-queue? q) 
    '()
    (helper (front-ptr q))))


(define q2 (make-queue))
(insert-queue! q2 'a)
(insert-queue! q2 'b)
(print-queue q2)

; 3.22

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (set-front-ptr! q new-pair)
      (set! front-ptr new-pair))
    (define (set-rear-ptr q new-pair)
      (set! rear-ptr new-pair))
    (define (insert-queue! item) 
      (let ((new-pair (cons item '()))) 
        (cond ((empty-queue?) 
               (set-front-ptr! new-pair) 
               (set-rear-ptr! new-pair)) 
              (else  
                (set-cdr! rear-ptr new-pair) 
                (set-rear-ptr! new-pair)))
        front-ptr))
    (define (print-queue) front-ptr)
    (define (dispatch m)
      (cond 
        ((eq? m 'print) print-queue)
        ((eq? m 'insert) (lambda (v)  (insert-queue! v)))
        )
      )
    dispatch))

(define (insert-queue! q v) ((q 'insert ) v))
(define (print-queue q) ((q 'print )))

(define q3 (make-queue))
(insert-queue! q3 'a)
(insert-queue! q3 'b)
(print-queue q3)
