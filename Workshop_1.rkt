#lang racket
; Calderón Prieto Brandon (2125874)
; Carlos Daniel Corrales (2122878)
; Melo Burbano Deisy (2041790)

; --------------------------- AUXILIAR FUNCTIONS --------------------------- ;

; Contract: L1, L2 -> L
; Purpose: returns the concatenation of "L1" and "L2".
; <list> := ()
;        := (<scheme value> <list>)

(define concat (
	lambda (l1 l2) (
		cond
			[(empty? l1) l2]
			[else (cons (car l1) (concat (cdr l1) l2))]
	)
))


; Contract: E, L -> L
; Purpose: returns "L1", but adding the element "E" in the last position of the list.
; <list> := ()
;        := (<scheme value> <list>)

(define add (
	lambda (element list) (
		cond
			[(empty? list) (cons element empty)]
			[else (cons (car list) (add element (cdr list)))]
	)
))

; -------------------------------------------------------------------------- ;


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

; Exercise 5
; Contract: P, L -> L
; Purpose: Returns (counting from position 0) the first element in the list that satisfy the
; predicate "p"
; <list> := ()
;        := (<scheme value> <list>)

(define (list-index p list)
	(define (aux index list)
		(cond
			[(empty? list) #f]
			[(p (car list)) index]
			[else (aux (+ index 1) (cdr list))]
			)
		)
	(aux 0 list)
)

; (list-index number? '(a 2 (1 3) b 7)) -> 1
; (list-index symbol? '(a (b c) 17 foo)) -> 0
; (list-index symbol? '(1 2 (a b) 3)) -> #f


; Exercise 6
; Contract: E1, E2, L -> L
; Purpose: The function returns a list L, and each occurrence of E1 is replaced by E2 and each
; occurrence of E1 is replaced by E2.
; <list> := ()
;        := (<scheme value> <list>)

(define (mapAux transform-func list)
	(if (empty? list)
		'()
		(cons (transform-func (car list)) (mapAux transform-func (cdr list)))
	)
)

(define (swapper e1 e2 list)
	(define (swap-element x)
		(cond
			[(equal? x e1) e2]
			[(equal? x e2) e1]
			[else x])
			)
	(mapAux swap-element list)
)

; (swapper 'a 'd '(a b c d)) -> (d b c a)
; (swapper 'a 'd '(a d () c d)) -> (d a () c a)
; (swapper 'x 'y '(y y x y x y x x y)) -> (x x y x y x y y x)



; Exercise 16
; Contract: L -> N
; Purpose: returns the result of performing the corresponding addition, subtraction and multiplication operations.
; <binary-operation> ::= <int>
;                    ::= (<binary-operation> 'suma <binary-operation>)
;                    ::= (<binary-operation> 'resta <binary-operation>)
;                    ::= (<binary-operation> 'multiplica <binary-operation>)

(define Operar-binarias (
	lambda (operation) (
		cond
			[(number? operation) operation]
			[(eqv? (cadr operation) 'suma) (+ (Operar-binarias (car operation)) (Operar-binarias (caddr operation)))]
			[(eqv? (cadr operation) 'resta) (- (Operar-binarias (car operation)) (Operar-binarias (caddr operation)))]
			[(eqv? (cadr operation) 'multiplica) (* (Operar-binarias (car operation)) (Operar-binarias (caddr operation)))]
	)
))

; (Operar-binarias '(2 suma 9))
; (Operar-binarias '(2 resta 9))
; (Operar-binarias '(2 multiplica 9))
; (Operar-binarias '( (2 multiplica 3) suma (5 resta 1)))
; (Operar-binarias '( (2 multiplica (4 suma 1)) multiplica ((2 multiplica 4) resta 1) ))




; Exercise 18
; Contract: N -> L
; Purpose: returns row "N" of Pascal's triangle.
; <list> := ()
;        := (<scheme value> <list>)

(define pascal (
	lambda (n) (
		letrec (
			(get-row (
				lambda (n actual-row) (
					if (= n 1)
						actual-row
						(get-row (- n 1) (zip + (cons 0 actual-row) (add 0 actual-row)))
				)
			))
		)
		(get-row n (cons 1 empty))
	)
))

; (pascal 5)
; (pascal 1)