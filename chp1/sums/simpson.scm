(define (cube x) (* x x x))
; assume n is even
(define (simpson-integral f a b N)
	(define h (/ (- b a) N))
	(define (y n) 
	  		(f (+ a (* n h))))
	(define (inc n) 
	  		(+ n 1))
	(define (seq-term k)
	  	(*	(cond ((or (= k 0) (= k N)) 1)
				  ((even? k) 2)
				  (else 4))
			(y k)))
	(* (/ h 3) (sum seq-term 0 inc N))
)
; higher-order procedure that defines a partial sum of a given sequence
; term is each term of the sequence
; next defines the increment
(define (sum term a next b)
  	(if (> a b)
	  	0
	  	(+ (term a)
		   (sum term (next a) next b))))
