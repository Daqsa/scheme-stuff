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
  	(if (fast-prime? n 50)
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

(define (fast-prime? n times)
	(cond ((= times 0) #t)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else #f)
	)
)

(define (fermat-test n)
  	(define (try-it a)
	  	(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (divides? a b)
	(= (remainder b a) 0)
)

(define (expmod base exponent m)
  	(cond ((= exponent 0) 1)
		  ((even? exponent)
		   (remainder 
			 (square (expmod base (/ exponent 2) m))
			m))
		  (else 
			(remainder
			  (* base (expmod base (- exponent 1) m))
			  m))))
