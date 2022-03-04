; block structure. 
; the advantage is all sub-procedures are local and abstracted away from the user
(define (cbrt x)

	; want to approximate cbrt(x) using Newton's method
	(define (better-cbrt y)
		(/  (+  (/ 	x 
					(square y)) 
				(* 2 y))
			3))

	(define epsilon 1e-5)

	(define (square n) (* n n))

	; check if y is within epsilon fraction of x
	(define (is-within a b)
	  (< (/ (abs (- a b)) a) epsilon))


	; stop when the new guess didn't change too much from old guess
	(define (iterate-cbrt y)
		(if (is-within y (better-cbrt y))
			y
			(iterate-cbrt (better-cbrt y))))

	; we define above procedures to call this procedure
	; notice we don't have to supply x again
	(iterate-cbrt 1.0)

)
