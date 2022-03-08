(define (search-for-primes n)
  	(if (odd? n)
		(search-for-primes-iter n 0)
		(search-for-primes-iter (+ n 1) 0)
	)
)

; Finds three smallest primes larger than n
; assume n is odd
(define (search-for-primes-iter n prime-count)
  	(if (> prime-count 2)
		#t
		(if (timed-prime-test n)
			(search-for-primes-iter (+ n 2) (+ prime-count 1))
			(search-for-primes-iter (+ n 2) prime-count)
		)
	)	
)


(define (timed-prime-test n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  	(if (prime? n)
	  	(report-prime n (- (runtime) start-time))
		#f
	)
)

(define (report-prime prime elapsed-time)
		(display " Prime: ")
		(display prime)
		(display ", time: ")
		(display elapsed-time)
		(newline)
		#t
)

(define (prime? n)
  	(= (smallest-prime n 2) n)
)

; assume n > 1
(define (smallest-prime n i)
	(if (> (square i) n) 
	  	n
		(if (divides? i n)
		  	i
			(smallest-prime n (next i))
		)
	)
)

(define (next test-divisor)
  	(if (= test-divisor 2)
	  	3
		(+ test-divisor 2)
	)
)


(define (divides? a b)
	(= (remainder b a) 0)
)
