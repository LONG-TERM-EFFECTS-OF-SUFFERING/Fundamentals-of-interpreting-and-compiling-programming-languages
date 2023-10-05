; Calderón Prieto Brandon (2125874)
; Corrales Carlos Daniel (2122878)
; Melo Burbano Deisy (2041790)

#lang eopl
(require "Exercise_1.rkt")


; --------------------------- AUXILIAR FUNCTIONS --------------------------- ;

; Contract: L -> L
; Purpose: returns the list "list" filtered based on a given predicate (a
; function that takes an element of the list and returns a boolean value).

(define my-filter (
	lambda (predicate list) (
		cond
			[(empty? list) empty]
			[(predicate (car list)) (cons (car list) (my-filter predicate (cdr list)))]
			[else (my-filter predicate (cdr list))]
	)
))


; Contract: L -> L
; Purpose: returns the list "list" with the result of applying the unary function
; "function" to each element of the list.

(define my-map (
	lambda (function list) (
		cond
			[(empty? list) empty]
			[else (cons (function (car list)) (my-map function (cdr list)))]
	)
))


; Contract: L -> L
; Purpose: returns the list "list" with the symbol "symbol" inserted between each element

(define join (
	lambda (symbol list) (
		cond
			[(empty? (cdr list)) (cons (car list) empty)]
			[else (cons (car list) (cons symbol (join symbol (cdr list))))]
	)
))

; ----------------------------------- OR ----------------------------------- ;

(define unparse-or-expression (
	lambda (exp) (join 'v (or-list->vars exp))
))

(define parse-or-expresion (
	lambda (exp) (
		or-list (my-filter (lambda (element) (
			not (eqv? element 'v)
		)) exp)
	)
))

; (unparse-or-expression (parse-or-expresion (list 1 'v 2 'v 3 'v 4)))
; (unparse-or-expression (parse-or-expresion (list 5 'v 6 'v 7 'v 8)))
; (unparse-or-expression (parse-or-expresion (list 9 'v 10 'v 11 'v 12)))

; ----------------------------------- AND ---------------------------------- ;

(define unparse-and-expression (
	lambda (exp) (
		let (
			(unparsed-clauses (my-map (lambda (element) (
					cond
					[(number? element) element]
					[(or-list? element) (unparse-or-expression element)]
				)) (and-list->clauses exp))
			)
		)
		(join '^ unparsed-clauses)
	)
))

(define parse-and-expresion (
	lambda (exp) (
		let* (
			(clauses (my-filter (lambda (element) (
				not (eqv? element '^)
			)) exp))

			(parsed-clauses (my-map (lambda (element) (
					cond
					[(number? element) element]
					[(list? element) (parse-or-expresion element)]
				)) clauses)
			)
		)
		(and-list parsed-clauses)
	)
))

; (unparse-and-expression (parse-and-expresion (list 1 '^ 2 '^ (list 3 'v 4) '^ 5 '^ (list 6 'v 7 'v 8))))
; (unparse-and-expression (parse-and-expresion (list (list 1 'v 2 'v 3 'v 4) '^ (list 5 'v 6 'v 7 'v 8))))
; (unparse-and-expression (parse-and-expresion (list (list 1 'v 2 'v 3 'v 4) '^ (list 5 'v 6 'v 7 'v 8) '^ 10 '^ 11 '^ 12 '^ 13)))

; ----------------------------------- FNC ---------------------------------- ;

(define unparse-fnc-expression (
	lambda (exp) (
		cond
			[(fnc-list? exp) (list 'FNC (fnc-list->vars exp) (unparse-and-expression (fnc-list->and-operation exp)))]
			[else eopl:error "unparparsing a no FNC expression"]
	)
))


(define parse-fnc-expresion (
	lambda (exp) (
		cond
			[(eqv? (car exp) 'FNC) (fnc-list (cadr exp) (parse-and-expresion (caddr exp)))]
			[else eopl:error "parsing a no FNC expression"]
	)
))

; (x OR NOT y OR z OR w) AND (NOT x OR NOT y OR NOT z) AND w, x = 1, y = 2, z = 3 and w = 4
; (unparse-fnc-expression (parse-fnc-expresion (list 'FNC 4 (list (list 1 'v -2 'v 3 'v 4) '^ (list -1 'v -2 'v -3) '^ 4))))


; a AND b AND c AND (d OR e OR f), a = 1, b = 2, c = 3, d = 4, e = 5 and f = 6

; (unparse-fnc-expression (parse-fnc-expresion (list 'FNC 4 (list 1 '^ 2 '^ 3 '^ (list 4 'v 5 'v 6)))))


; (a OR b OR c) AND d AND e AND f, a = 1, b = 2, c = 3, d = 4, e = 5 and f = 6

; (unparse-fnc-expression (parse-fnc-expresion (list 'FNC 4 (list (list 1 'v 2 'v 3) '^ 4 '^ 5 '^ 6))))
