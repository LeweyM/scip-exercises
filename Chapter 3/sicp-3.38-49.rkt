(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire)))
            ((eq? m 'release)  (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
    true
    (begin (set-car! cell true)
           false)))

; 3.38

; a.
; +10,-20,/2 => 45$
; -20,+10,/2 => 45$
; +10,/2,-20 => 35$
; -20,/2,+10 => 50$
; /2,+10,-20 => 40$
; /2,-20,+10 => 40$

;b. example of interleaved execution

; peter: balance:100
; paul: balance:100
; mary: balance:100
; peter: balance:110
; paul: balance:80
; mary: balance:50
; peter: set! balance 110
; paul set! balance 80
; mary set! balance 50
; balance => 50

; 3.39

; 100 => determine x2=x*x=100, set! increment x to 11, then set! x to x2 
; 101 => determine x2=x*x=100, set! x to x2, then set! increment x to 101
; 121 => set! increment x to 11, determine x2=x*x=121, then set! x to x2

; 3.40

; B(set! x A(* x x))
; D(set! x C(* x x x)) 

; constraints on execution order
; A > B
; C > D

; ABCD => A:100 -> B:x=100 -> C:1000000 -> D:x=1000000
; ACBD => A:100 -> C:1000 -> B:x=100 -> D:x=1000 => 1000
; ACDB => A:100 -> C:1000 -> D:x=1000 -> B:x=100 => 100
; CDAB => C:1000 -> D:x=1000 -> A:1000000 -> B:x=1000000 => 10000000
; CADB => C:1001 -> A:100 -> D:x=1000 -> B:x=100 => 100

; serializing means that AB and CD must be run together. The only possibilities are ABCD and CDAB. Both of these results in 1000000.
; note: as multiplication is an additive operation, the order will not matter.

; 3.41

; In the case that balance returns a symbol (a number) and not a reference, it should be safe to return it.
; As long as the write operations are serialized, the read will only give the balance before or after a transaction, which is normal behaviour. If it could provide an interim state, that would be an issue. 

; 3.42

; This is difficult to answer without knowing the implementation details of the serializer. In theory, there could be a difference as each serialized function will be referentially equal. If this is used to determine sameness, this could lead to interleaving.

; 3.43

; As long as both the actual operations (deposit and withdraw) and the exchange operations are serialized, the swaps themselves will maintain consistency, although the order will differ.

; example:
; A:10,B:20,C:30 => A<->B, B<->C => A:20,B:30,C:10
; A:10,B:20,C:30 => B<->C, A<->B => A:30,B:10,C:20 

; on the other hand, even if the order of the transactions is guarenteed, if the operations themselves are not serialized they could still interleave between read and writes and lose the consistency of the total values.

;example:
; A:10,B:20,C:30 => A<->B
; a = read A => 10
; b = read b => 20
; set! A b => A => 20
; set! B a => 10
; or
; a = read A => 10
; set! B a => 10
; b = read B => 10
; set! A b => 10

; 3.44

; The difference between transfer and exchange is that each account in an exchange operation requires information on the state of the other account. This is not the case for a transfer operation as they simply need to know an amount, which remains constant. In theory, there is no requirement for the operations to take place at the same time at all.

; 3.45

; The end result would be that some operations would be 'double wrapped' with serializers. Again, it's hard to say what the net effect of this would be without knowing the details of the serializer implementation, but it would probably lead to the process being unable to run as it would wait forever for the outer serializer.

; 3.46

; Each mutex has a read and write operation, (car cell) and (set-car! cell true) respectively. These can be interleaved if not made atomic, resulting in multiple concurrent executions.

; mutex - #f
; A calls test-and-set!
; A reads #f
; B calls test-and-set!
; B reads #f
; A executes process and sets #t 
; B executes process and sets #t 

; 3.47

(define (make-semaphore n) 
  (let ((total 0) 
        (access-lock (make-mutex))) 
    (define (acquire) 
      (access-lock 'acquire) 
      (if (< total n) 
        (begin (set! total (+ total 1)) 
               (access-lock 'release)) 
        (begin (access-lock 'release) 
               (acquire)))) 
    (define (release) 
      (access-lock 'acquire) 
      (set! total (- total 1)) 
      (access-lock 'release)) 
    (define (the-semaphore m) 
      (cond ((eq? m 'acquire)  (acquire)) 
            ((eq? m 'release)  (release)))))) 

; 3.48

; Deadlocks occur because two processes are waiting for complementary resources. That is to say that p1 has locked r1 and is waiting for r2 and p2 has locked r2 and is waiting for r1. By insuring that processes always access shared resources in the same order, we can guarentee that this will not occur. For example, p1 will lock r1 and wait for r2, and p2 will try to access r1 also but will have to wait until p1 has finished. The implementation in this case is straight forward.

(define (make-account-and-serializer id balance) 
  (define (withdraw amount) 
    (if (>= balance amount) 
      (begin (set! balance (- balance amount)) 
             balance) 
      "Insufficient funds")) 
  (define (deposit amount) 
    (set! balance (+ balance amount)) 
    balance) 
  (let ((balance-serializer (make-serializer))) 
    (define (dispatch m) 
      (cond ((eq? m 'id) id) 
            ((eq? m 'withdraw) withdraw) 
            ((eq? m 'deposit) deposit) 
            ((eq? m 'balance) balance) 
            ((eq? m 'serializer) balance-serializer) 
            (else (error "Unknown request -- MAKE-ACCOUNT" 
                         m)))) 
    dispatch)) 

(define (serialized-exchange account1 account2) 
  (let* ((serializer1 (account1 'serializer)) 
         (serializer2 (account2 'serializer)) 
         (exchanger (if (< (account1 'id)  (account2 'id)) 
                      (serializer1 (serializer2 exchange)) 
                      (serializer2 (serializer1 exchange))))) 
    (exchanger account1 account2))) 

; 3.49

; In cases in which we do not know beforehand the two resources which will be operated on, we cannot guarentee the ordering and therefore cannot guarentee a non-symetrical execution which results in a deadlock. 
; The types of situations in which this could occur is when one of the resources is selected dynamically as a result of data extracted from another resource. For example, if we wanted a function that gives 10$ to any other account which has the same balance as my account, we wouldn't know which is the target account until my account has first been accessed and evaluated.
