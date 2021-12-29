(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records))  (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value)  (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;; two-dimensional
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          false))
      false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable
                    (cons (cons key-2 value)
                          (cdr subtable)))))
      (set-cdr! table
                (cons (list key-1
                            (cons key-2 value))
                      (cdr table)))))
  'ok)

;; local tables
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; 3.24

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records))  (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))
    (define (print) local-table)
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) print)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table eq?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print (operation-table 'print-proc))

(put 0 4 'a)
(put 0 7 'b)
(get 0 7)
(print)

; 3.25

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (let ((subtable local-table))
        (define (helper l)
          (cond
            ((null? l) 'bad)
            ((null? (cdr l)) 
             (let ((record (assoc (car l) (cdr subtable))))
               (if record
                 (cdr record)
                 #f)))
            (else (let ((t (assoc (car l) (cdr subtable))))
                    (if t
                      (begin (set! subtable t) (helper (cdr l)))
                      #f)))))
        (helper keys)))
    (define (insert! keys value)
      (let ((subtable local-table))
        (define (helper l)
          (cond
            ((null? l) 'bad)
            ((null? (cdr l)) 
             (let ((record (assoc (car l) (cdr subtable))))
               (if record
                 (set-cdr! record value)
                 (set-cdr! subtable
                           (cons (cons (car l) value)
                                 (cdr subtable))))))
            (else (begin 
                    (let ((t (assoc (car l) (cdr subtable))))
                      (if t
                        (set! subtable t)
                        (set-cdr! subtable
                                  (cons (cons (car l) '())
                                        (cdr subtable)))))
                    (helper (cdr l))))))
        (helper keys))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table ))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put '(0 4) 'a)
(get '(0 4))

; 3.26

(define (make-table)
  ; node
  (define (make-node key value) (list (make-entry key value) '() '()))
  (define (get-entry node) (car node)) 
  (define (left node) (cadr node)) ; left node
  (define (right node) (caddr node)) ; right node
  (define (set-left! node new-node) (set-car! (cdr node) new-node))
  (define (set-right! node new-node) (set-car! (cddr node) new-node))

  ;entry
  (define (make-entry key value) (cons key value))
  (define (get-key entry) (car entry))
  (define (get-value entry) (cdr entry))
  (define (set-value! entry value) (set-cdr! entry value))

  (let ((local-table '()))

    (define (lookup key node)
      (cond ((null? node) #f)
            ((= key (get-key (get-entry node))) (get-entry node))
            ((> key (get-key (get-entry node))) (lookup key (left node)))
            ((< key (get-key (get-entry node))) (lookup key (right node)))))

    (define (attach-node new-node node)
      (cond ((null? local-table) (set! local-table new-node))
            ((> (get-key (get-entry new-node)) (get-key (get-entry node)))
             (if (null? (left node))
               (set-left! node new-node)
               (attach-node new-node (left node))))
            ((< (get-key (get-entry new-node)) (get-key (get-entry node)))
             (if (null? (right node))
               (set-right! node new-node)
               (attach-node new-node (right node))))))

    (define (insert key value)
      (let ((entry (lookup key local-table)))
        (if entry
          (set-value! entry value)
          (attach-node (make-node key value) local-table))))

    (define (find key)
      (lookup key local-table))

    (define (dispatch m)
      (cond ((eq? m 'put) insert)
            ((eq? m 'get) find)
            ((eq? m 'print) (display local-table))))

    dispatch))

(define t (make-table))
((t 'put) 5 'a)
((t 'put) 8 'b)
((t 'put) 1 'c)
((t 'get) 5)
(t 'print)

; 3.27

; This only makes sense if the (make-table) function produces a global table which is always referenced by all calls to 'make-table'. Otherwise, a new table is created at each level of the recursion and no useful computation is memoized. In the case that it is global, and assuming an efficient table implementation, memoization will ensure that each (fib n) for n will be computed only once, and then simply referenced from the table afterwards. Therefore, the computation tree will not be evaluated, only one path from top to bottom. This will be the depth of the tree, which has size n.

; In this case, useing (memoize fib) would not work as fib would call itself at each recursion, instead of calling itself inside memoize which would do the lookup logic.


