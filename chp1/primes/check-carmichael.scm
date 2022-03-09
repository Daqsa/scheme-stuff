(define (passes-fermat n)
  	(if (fast-prime? n (- n 1))
	  	#t
		#f))

(define (fast-prime? n times)
	(cond ((= times 0) #t)
		((fermat-test n times) (fast-prime? n (- times 1)))
		(else #f)
	)
)

(define (fermat-test n a)
		(= (expmod a n n) a))

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
