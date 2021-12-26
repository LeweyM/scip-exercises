; 3.9

; recursive factorial

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

; iterative factorial

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

#|
recursive environments: (factorial 6)

E1: n:6,
E2: n:5,
E2: n:4,
E2: n:3,
E2: n:2,
E2: n:1,

iterative environments: (factorial 6)

factorial: 
E1: n:6

fact-iter:
E1: product:1, counter:1, n:6
E2: product:1, counter:2, n:6
E3: product:2, counter:3, n:6
E4: product:6, counter:4, n:6
E5: product:24, counter:5, n:6
E6: product:120, counter:6, n:6
E7: product:720, counter:7, n:6

|#
