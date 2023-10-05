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
			[(null? list) empty]
			[(predicate (car list)) (cons (car list) (my-filter predicate (cdr list)))]
			[else (my-filter predicate (cdr list))]
	)
))

; run the following tests
; the '->' symbol indicates the test output
; (my-filter even? '(1 2 3 4 5 6)) -> (2 4 6)
; (my-filter string? '(1 "two" 3 "four" 5 6)) -> ("two" "four")
; (my-filter (lambda (x) (> x 10)) '(5 15 20 25)) -> (15 20 25)


; ---------------------------
; Contract: L -> L
; Purpose: returns the list "list" with the result of applying the unary function
; "function" to each element of the list.

(define my-map (
	lambda (function list) (
		cond
			[(null? list) empty]
			[else (cons (function (car list)) (my-map function (cdr list)))]
	)
))

; run the following tests
; the '->' symbol indicates the test output
; (my-map (lambda (x) (+ x 2)) '(5 4 6 4 110)) -> (7 6 8 6 112)
; (my-map (lambda (x) (* x 2)) '(2 4 6 8 10)) -> (4 8 12 16 20)
; (my-map string-length '("apple" "banana" "cherry")) -> (5 6 6)


; Contract: L -> L
; Purpose: returns the list "list" with the symbol "symbol" inserted between each element

(define join (
	lambda (symbol list) (
		cond
			[(null? (cdr list)) (cons (car list) empty)]
			[else (cons (car list) (cons symbol (join symbol (cdr list))))]
	)
))

; run the following tests
; the '->' symbol indicates the test output
; (join '- '(1 2 3 4 5)) -> (1 - 2 - 3 - 4 - 5)
; (join '_ '(x y z))     -> (x _ y _ z)
; (join '?? '(x y z))    -> (x ?? y ?? z)


; ----------------------------------- OR ----------------------------------- ;

; Contract: L -> L
; Purpose: This function converts an exp expression to a string representation where 
; elements are joined with the 'v' symbo

(define unparse-or-expression (
	lambda (exp) (join 'v (or-list->vars exp))
))
; run the following tests
; the '->' symbol indicates the test output
; (parse-or-expresion (list 1 'v 2 'v 3 'v 4))    ->  (OR (1 2 3 4))
; (parse-or-expresion (list 5 'v 6 'v 7 'v 8))    ->  (OR (5 6 7 8))
; (parse-or-expresion (list 9 'v 10 'v 11 'v 12)) ->  (OR (9 10 11 12))



; Contract: L -> L
; Purpose: Filters an exp expression, removing all elements that are equal to 'v'.

(define parse-or-expresion (
	lambda (exp) (
		or-list (my-filter (lambda (element) (
			not (eqv? element 'v)
		)) exp)
	)
))

; run the following tests
; the '->' symbol indicates the test output
; (unparse-or-expression (parse-or-expresion (list 1 'v 2 'v 3 'v 4)))     ->   (1 v 2 v 3 v 4)
; (unparse-or-expression (parse-or-expresion (list 5 'v 6 'v 7 'v 8)))     ->   (5 v 6 v 7 v 8)
; (unparse-or-expression (parse-or-expresion (list 9 'v 10 'v 11 'v 12)))  ->   (9 v 10 v 11 v 12)

; ----------------------------------- AND ---------------------------------- ;

; Contract: L -> L
; Purpose: Converts an 'exp' expression to a string representation where subexpressions of 'or'
; are joined with the '^' symbol.
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
; run the following tests
; the '->' symbol indicates the test output
; (unparse-and-expression (parse-and-expresion (list 1 '^ 2 '^ (list 3 'v 4) '^ 5 '^ (list 6 'v 7 'v 8))))
; (unparse-and-expression (parse-and-expresion (list (list 1 'v 2 'v 3 'v 4) '^ (list 5 'v 6 'v 7 'v 8))))
; (unparse-and-expression (parse-and-expresion (list (list 1 'v 2 'v 3 'v 4) '^ (list 5 'v 6 'v 7 'v 8) '^ 10 '^ 11 '^ 12 '^ 13)))


; Contract: L -> L
; Purpose: Filters an exp expression, removing the '^' symbol and parsing 'or' subexpressions.
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
; run the following tests
; the '->' symbol indicates the test output
; (parse-and-expresion (list 1 '^ 2 '^ (list 3 'v 4) '^ 5 '^ (list 6 'v 7 'v 8)))  ->  (AND (1 2 (OR (3 4)) 5 (OR (6 7 8))))
; (parse-and-expresion (list (list 1 'v 2 'v 3 'v 4) '^ (list 5 'v 6 'v 7 'v 8)))  ->  (AND ((OR (1 2 3 4)) (OR (5 6 7 8))))
; (parse-and-expresion (list (list 1 'v 2 'v 3 'v 4) '^ (list 5 'v 6 'v 7 'v 8) '^ 10 '^ 11 '^ 12 '^ 13))  ->  (AND ((OR (1 2 3 4)) (OR (5 6 7 8)) 10 11 12 13))

; ----------------------------------- FNC ---------------------------------- ;

; Contract: L -> L
; Purpose: Converts an exp expression to a string representation for Conjunctive Normal Form (CNF) by prefixing 'CNF'
; and joining variables with 'v' and clauses with '^'.

(define unparse-fnc-expression (
	lambda (exp) (
		cond
			[(fnc-list? exp) (list 'FNC (fnc-list->vars exp) (unparse-and-expression (fnc-list->and-operation exp)))]
			[else eopl:error "unparparsing a no FNC expression"]
	)
))
; run the following tests
; the '->' symbol indicates the test output

; (x OR NOT y OR z OR w) AND (NOT x OR NOT y OR NOT z) AND w, x = 1, y = 2, z = 3 and w = 4
; (unparse-fnc-expression (parse-fnc-expresion (list 'FNC 4 (list (list 1 'v -2 'v 3 'v 4) '^ (list -1 'v -2 'v -3) '^ 4))))   ->  (FNC 4 ((1 v -2 v 3 v 4) ^ (-1 v -2 v -3) ^ 4))

; a AND b AND c AND (d OR e OR f), a = 1, b = 2, c = 3, d = 4, e = 5 and f = 6
; (unparse-fnc-expression (parse-fnc-expresion (list 'FNC 4 (list 1 '^ 2 '^ 3 '^ (list 4 'v 5 'v 6)))))                        ->  (FNC 4 (1 ^ 2 ^ 3 ^ (4 v 5 v 6)))

; (a OR b OR c) AND d AND e AND f, a = 1, b = 2, c = 3, d = 4, e = 5 and f = 6
; (unparse-fnc-expression (parse-fnc-expresion (list 'FNC 4 (list (list 1 'v 2 'v 3) '^ 4 '^ 5 '^ 6))))                        ->  (FNC 4 ((1 v 2 v 3) ^ 4 ^ 5 ^ 6))
 

(define parse-fnc-expresion (
	lambda (exp) (
		cond
			[(eqv? (car exp) 'FNC) (fnc-list (cadr exp) (parse-and-expresion (caddr exp)))]
			[else eopl:error "parsing a no FNC expression"]
	)
))
; run the following tests
; the '->' symbol indicates the test output
; (parse-fnc-expresion (list 'FNC 4 (list (list 1 'v -2 'v 3 'v 4) '^ (list -1 'v -2 'v -3) '^ 4)))                ->   (FNC 4 (AND ((OR (1 -2 3 4)) (OR (-1 -2 -3)) 4)))
; (parse-fnc-expresion (list 'FNC 4 (list 1 '^ 2 '^ 3 '^ (list 4 'v 5 'v 6))))                                     ->   (FNC 4 (AND (1 2 3 (OR (4 5 6)))))
; (parse-fnc-expresion (list 'FNC 4 (list (list 1 'v 2 'v 3) '^ 4 '^ 5 '^ 6)))                                     ->   (FNC 4 (AND ((OR (1 2 3)) 4 5 6)))
