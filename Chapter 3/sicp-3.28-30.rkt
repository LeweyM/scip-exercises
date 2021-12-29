;: (define a (make-wire))
;: (define b (make-wire))
;: (define c (make-wire))
;: (define d (make-wire))
;: (define e (make-wire))
;: (define s (make-wire))
;: 
;: (or-gate a b d)
;: (and-gate a b c)
;: (inverter c e)
;: (and-gate d e s)


;;NB. To use half-adder, need or-gate from exercise 3.28
(define (half-adder a b s c)
  (let ((d (make-wire))  (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and x y)
  (if (and (= x 1)  (= y 1))
    1
    0))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1)  (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; 3.28

(define (logical-or x y)
  (cond ((= x 1) 1)
        ((= y 1) 1)
        (else 0)))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1)  (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; 3.29

(define (compound-or-gate a1 a2 output)
  (let ((b1 (make-wire)) 
        (b2 (make-wire))
        (andout (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 andout)
    (inverter andout output)
    'ok))

; the delay will here will be 2*inverter+and-gate.

; note: there is some debate online as to whether it would be 3 or 2 inverter delays. My logic here is that, assuming the input signals come in at the same time, the first inverters would be triggered at the same time and so we only have to wait the duration of one inverter-wait, not two.

; 3.30

(define (ripple-carry-adder A B S C)
  (define (iter a b s c)
    (if (null? a) 'ok ; all lists are of size k, so no need to check b and s.
      (let ((An (car a)) 
            (Arest (cdr a))
            (Bn (car b))
            (Brest (cdr b))
            (Sn (car s))
            (Srest (cdr s))
            (cout (make-wire)))
          (full-adder An Bn c Sn cout)
          (iter Arest Brest Srest cout))))
  (iter A B S C))

; as the components are connected sequentially, we can find the delay factor of ripple-carry-adder as k*full-adder 

; 3.31

; conceptually, 'accept-action-procedure!' is not the action execution itself, but rather its assigning it to the agenda to be executed at the correct time. The reason it also needs to be stored on the wire is those downstream actions also need to be executed should the signal on the wire change. 

; 3.32

; The lists in 'time-segement's represent snapshots of state, and their execution brings the system to the next snapshot in state. It is possible, in the case of the and-gate for example, that the interim states are incorrect. 

; (0, 1) => 0 ;; initial state
; (1, 1) => 1 ;; one change snapshot added to list. This is the interim state.
; (1, 0) => 0 ;; second change snapshot added to list. The final state.

; [(1, 1) => 1, (1, 0) => 0] ;; event list

; From the above description, we can see that we want to execute these snapshots in the same order, via a queue. Executing these in reverse order, with a stack, would end the system in the interim state and would therefore be invalid.
