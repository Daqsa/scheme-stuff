; Got help from
; http://community.schemewiki.org/?sicp-ex-1.28 


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
  	(if (fast-prime? n 100)
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
		((miller-test n) (fast-prime? n (- times 1)))
		(else #f)
	)
)

; we know that for each squaring step if nontrivial sqrt of 1 is found (1 or n-1),
; then n is not prime.
(define (miller-test n)
  	(define (try-it a)
	  	(= (miller-expmod a (- n 1) n) 1))
	(try-it (+ 1 (random (- n 1)))))

(define (divides? a b)
	(= (remainder b a) 0)
)

(define (miller-expmod base exponent m)
  	(define (expmod-check x)
		(define (check-nontrivial-sqrt1 x x-square)	
		  	(if (and (= x-square 1)
					 (not (= x 1))
					 (not (= x (- m 1))))
				0
				x-square))
		(check-nontrivial-sqrt1 x (remainder (square x) m))
	)
  	(cond ((= exponent 0) 1)
		  ((even? exponent)
				(expmod-check 
					(miller-expmod base (/ exponent 2) m))
				)
		  (else 
			(remainder
			  (* base (miller-expmod base (- exponent 1) m))
			  m))))
