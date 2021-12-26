; 3.12

; (cdr x) after 'append'
; (b->(NULL))

; (cdr x) after 'append!'
; (b->(c->(d->(NULL))))

; 3.13

; (a->b->c->a->...)
; attempting to find the last pair would result in an infinte loop as no pair has the cdr of NULL

;3.14

#|
iterations:

1. (x:(a b c d) y:()) => x:(a) temp:(b c d)
2. (x:(b c d) y:(a))  => x:(b a) temp:(c d)
3. (x:(c d) y:(a b))  => x:(c b a) temp:(d)
4. (x:(d) y:(a b c))  => x:(d c b a) temp:()

the function reversed the list by iteratively reassigning the pointer of the elements to the remainder of the cdr.

|#

;3.15

#|
z1
; (a->(b->LOOP TO a))

(set-to-wow! z1)
; ('wow->(b->LOOP TO a))

z2
(a->(b->(a2->(b2->NULL))))
; ('wow->(b->(a2->(b2->NULL))))
