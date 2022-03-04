; implementation of if using cond.
; are there any problems?

(define (new-if predicate then-clause else-clause)
  	(cond (predicate then-clause)
		  (else else-clause)))

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

; seems like it gets stuck on an infinite loop. 
; why does new-if cause this?
; Because the interpreter evaluates in normal order (expands everything then evaluate)
; and new-if is not if (which is built-in) so it's a procedure, and 
; in normal order procedures are fully expanded
; if is not a procedure, so it doesn't get expanded
(define (iterate-sqrt x y)
  	(new-if (is-close-enough x (square y)) 
	  	y
		(iterate-sqrt x 
					  (better-sqrt x y))))



