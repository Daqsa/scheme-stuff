(define (integral f a b dx)
  	(define (add-dx x)
	  	(+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b)
	   dx))
; higher-order procedure that defines a partial sum of a given sequence
; term is each term of the sequence
; next defines the increment
(define (sum term a next b)
  	(if (> a b)
	  	0
	  	(+ (term a)
		   (sum term (next a) next b))))
