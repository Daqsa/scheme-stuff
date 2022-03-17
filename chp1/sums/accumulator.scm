; if filter is true, accumulate
(define (filtered-accumulate combiner null-value term a next b filter)
  	(if (> a b)
	  	null-value
		(if (filter a)
			(combiner (term a)
					  (filtered-accumulate combiner null-value term (next a) next b filter))
		  	(filtered-accumulate combiner null-value term (next a) next b filter))))

(define (accumulate-recursive combiner null-value term a next b)
  	(if (> a b)
	  	null-value
		(combiner (term a)
				  (accumulate-recursive combiner null-value term (next a) next b))))

(define (accumulate-iterative combiner null-value term a next b)
	(define (iter combiner a result)
	  	(if (> a b)
		  	result
			(iter combiner (next a) (combiner (term a) result))))
	(iter combiner a null-value))

(define (sum term a next b)
  	(accumulate-iterative + 0 term a next b))

(define (product term a next b)
  	(accumulate-iterative * 1 term a next b))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial x) 
  	(product identity 1 inc x))

(define (summation x)
  	(sum identity 0 inc x))

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
  
; assume a >= b
(define (my-gcd a b)
  	(cond ((= b 0) a)
		  (else (my-gcd b (remainder a b)))))

(define (coprime? a b)
  	(= (my-gcd a b) 1))

; coprime product
(define (cpp n)
  	(define (coprime-with-n x)	(coprime? n x))
  	(filtered-accumulate * 1 identity 1 inc n coprime-with-n))
