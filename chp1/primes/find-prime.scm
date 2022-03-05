(define (prime? n)
  	(= (smallest-prime n 2) n)
)

; assume n > 1
(define (smallest-prime n i)
	(if (> (square i) n) 
	  	n
		(if (divides? i n)
		  	i
			(smallest-prime n (+ i 1))
		)
	)
)

(define (divides? a b)
	(= (remainder b a) 0)
)
