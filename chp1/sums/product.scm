; multiply f(n) for all a <= n <= b
(define (product-recursive term a next b)
  	(if (> a b)
	  	1
	  	(* (term a)
		   (product-recursive term (next a) next b))))

(define (product-iterative term a next b) 
  	(define (iter a result)
		(if	(> a b)
		  	result
			(iter (next a) (* (term a) result))))
	(iter a 1))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (pi-sum n)
  	(define (pi-term n)
	  	(/ (* (+ n 1.0) (- n 1.0))
		   (square n)))
	(define (pi-next n)
	  	(+ n 2))
	(* 4 (product-recursive pi-term 3 pi-next n)))

(define (factorial n)
  	(product-iterative identity 1 inc n))
