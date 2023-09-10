#lang racket
; Calderón Prieto Brandon (2125874)
; Carlos ()
; Deisy ()

; Exercise 1
; Contract: L -> L
; Purpose: returns "L", but with its inverted ordered pairs.
; <list> := ()
;        := (<scheme value> <list>)

(define invert (
	lambda (list) (
		let (
			(invert-pair (
				lambda (pair) (
					cons (cadr pair ) (cons (car pair) empty)
				)
			))
		)
		(cond
			[(empty? list) empty]
			[else (cons (invert-pair (car list)) (invert (cdr list)))])
	)
))

; (invert '((a 1) (a 2) (1 b) (2 b)))
; (invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))
; (invert '(("es" "racket") ("genial" "muy") (17 29) (81 0)))


; Exercise 2
; Contract: L -> L
; Purpose: returns a list with each element of "L" associated with one more level of parentheses
; compared to its original state in "L".
; <list> := ()
;        := (<scheme value> <list>)

(define down (
	lambda (list) (
		let (
			(add-parenthesis (
				lambda (element) (
					cons element empty
				)
			))
		)
		(cond
			[(empty? list) empty]
			[else (cons (add-parenthesis (car list)) (down (cdr list)))])
	)
))

; (down '((a 1) (a 2) (1 b) (2 b)))
; (down '((una) (buena) (idea)))
; (down '(un (objeto (mas)) complicado))


; Exercise 3
; Contract: L, N, X -> L
; Purpose: returns "L", but with element "x" at position "n".
; <list> := ()
;        := (<scheme value> <list>)

(define list-set (
	lambda (list n x) (
		if (= n 0)
			(cons x (cdr list))
			(cons (car list) (list-set (cdr list) (- n 1) x))
	)
))

; (list-set '(a b c d) 2 '(1 2))
; (list-set '(a b c d) 3 '(1 5 10))


; Exercise 4
; Contract: P, L -> L
; Purpose: returns a list that contains the elements that belong to "L" and satisfy the predicate "P".
; <list> := ()
;        := (<scheme value> <list>)

(define filter-in (
	lambda (p list) (
		cond
			[(empty? list) empty]
			[(p (car list)) (cons (car list) (filter-in p (cdr list)))]
			[else (filter-in p (cdr list))]
	)
))

; (filter-in number? '(a 2 (1 3) b 7))
; (filter-in symbol? '(a (b c) 17 foo))
; (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))
