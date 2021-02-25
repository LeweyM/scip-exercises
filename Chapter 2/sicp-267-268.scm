					; 2.67

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; '(a d a b b c a)

					; 2.68

					;at each node, if a leaf and correct symbol, return bits
					; if a leaf and wrong symbol, return false
					; if a node and symbol not in set, return false
					; else, store branch left
					; if branch left false, return branch right, else return branch left

(define (contains x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (contains x (cdr set)))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-helper t bits)
    (if (leaf? t)
	(if (eq? (symbol-leaf t) symbol)
	    (reverse bits) 
	    #f)
	(if (contains symbol (symbols t))
	    (let ((left-result (encode-symbol-helper (left-branch t) (cons 0 bits))))
	      (if (false? left-result)
		  (encode-symbol-helper (right-branch t) (cons 1 bits))
		  left-result))
	    #f)))
  (encode-symbol-helper tree '()))

					; This can be refactored and simplified by simply cons'ing the next bit to the result from the rest of the tree traversal.

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      (if (eq? (symbol-leaf tree) symbol)
	  '() 
	  (error '"doesnt exist"))
      (let ((lb (left-branch tree))
	    (rb (right-branch tree)))
	(if (contains symbol (symbols lb))
	    (cons 0 (encode-symbol symbol lb))
	    (cons 1 (encode-symbol symbol rb))))))

(encode-symbol 'd sample-tree)

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(encode '(a d a b b c a) sample-tree) ; '(0 1 1 0 0 1 0 1 0 1 1 1 0)

					; 2.69

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge
       (adjoin-set
	(make-code-tree (car leaf-set) (cadr leaf-set))
	(cddr leaf-set)))))

(define my-tree (generate-huffman-tree '((A 4) (B 3) (C 1) (D 1))))

(encode '(a d a b b c a) my-tree)

					; 2.70

(define song-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))

(define lyrics '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))

(define encoded-song (encode lyrics song-tree)) ; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

(assert (equal? (decode encoded-song song-tree)
		lyrics))

(length encoded-song) ; 84
(* 3 (length lyrics)) ; 36 * 3 -> 108

					; to represent a message of n symbols with fixedlength bitvectors, we would need x bits where 2^x is greater than n - or 2log(n). In this case with 8 symbols, we would need 3 bits per symbol. For 36 symbols this would be 108 bits.

					; 2.71

(define alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define (pow-2-symbol-pairs final alphabet)
  (define (iter n symbols)
    (if (= n final)
	'()
	(cons (list (car symbols) (expt 2 n))
	      (iter (+ n 1) (cdr symbols)))))
  (iter 0 alphabet))

(pow-2-symbol-pairs 10 alphabet)

					; The sum of the symbol weights from 0 to n-1 is n weight - 1, so the weight at n is always greater than the combined sum of the lower weighted symbols. 

(generate-huffman-tree (pow-2-symbol-pairs 5 alphabet))

#|

(((((leaf a 1)
(leaf b 2) (a b) 3)
(leaf c 4) (a b c) 7)
(leaf d 8) (a b c d) 15)
(leaf e 16) (a b c d e) 31)

                     {a b c d e} 31
                     /           \
                {a b c d} 15      e 16
                 /     \
           {a b c} 7    d 8
             /    \
        {a b} 3    c 4
         /   \
      a 1    b 2

|#

(generate-huffman-tree (pow-2-symbol-pairs 10 alphabet))

|# 

((((((((((leaf a 1)
	 (leaf b 2) (a b) 3)
	(leaf c 4) (a b c) 7)
       (leaf d 8) (a b c d) 15)
      (leaf e 16) (a b c d e) 31)
     (leaf f 32) (a b c d e f) 63)
    (leaf g 64) (a b c d e f g) 127)
   (leaf h 128) (a b c d e f g h) 255)
  (leaf i 256) (a b c d e f g h i) 511)
 (leaf j 512) (a b c d e f g h i j) 1023)

#|

					; The most frequent symbol on this tree will always be the first leaf, and so will be encoded with 1 bit. The least frequent symbol will have to traverse through the other symbol sets, so will require n-1 bits.

					; 2.72

					; The steps for encoding a symbol:

					; Check if left-branch contains a symbol O(n) where n is between 1-n. N will decrease at each branch, for a balanced tree this will decrease by a factor of 2, for a worst case tree, this will decrease by 1.
					; For a balanced tree, the proc is called log(n) times. At each call, we search a set of symbols which is halfing every time.
