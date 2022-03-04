(define (average x y) 
  	(/ (+ x y) 2))

; want to approximate sqrt(x) using Newton's method
; get a slightly better approximation by taking avg(y, x/y)
(define (better-sqrt x y)
  	(average y (/ x y)))

(define epsilon 1e-5)

(define (square x) (* x x))

(define (is-close-enough x y) 
  (< (abs (- x y)) epsilon))

(define (iterate-sqrt x y)
  	(if (is-close-enough x (square y)) 
	  	y
		(iterate-sqrt x 
					  (better-sqrt x y))))


