; 3.5 

(define (square x) (* x x))

(define (random-in-range low high) 
  (let ((range (- high low))) 
    (+ low (* (random) range)))) 

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)  (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (circle-in-rect-experiment P x1 x2 y1 y2)
  (lambda () (let ((randx (random-in-range x1 x2))
                   (randy (random-in-range y1 y2)))
               (P randx randy)) ))

(define (estimate-integral P x1 x2 y1 y2 trails)
  (let ((area (* (- x2 x1) (- y2 y1))))
    (* area (monte-carlo trails (circle-in-rect-experiment P x1 x2 y1 y2)))))

(estimate-integral (lambda (x y)  (<= (+ (* x x)  (* y y)) 1.0)) -1.0 1.0 -1.0 1.0 100000) ;3.14392

; 3.6

(define (make-rand)
  (let ((state 1))
     (lambda (m) 
       (cond 
         ((equal? m 'generate) (begin (set! state (modulo (+ 2 (* 3 state)) 20)) state))
         ((equal? m 'reset) (set! state 1))
         (else (error "message not recognized"))))))

(define rand (make-rand))
(rand 'generate) ; some random number - r
(rand 'generate) ; another random number
(rand 'generate) ; another random number
(rand 'generate) ; another random number
(rand 'reset)
(rand 'generate) ; same as first random number r
