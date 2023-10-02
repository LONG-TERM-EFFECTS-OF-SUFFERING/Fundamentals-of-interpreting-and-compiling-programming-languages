; Calderón Prieto Brandon (2125874)
; Corrales Carlos Daniel (2122878)
; Melo Burbano Deisy (2041790)

#lang eopl


; -------------------------------------------------------------------------- ;
;                                 BNF GRAMMAR                                ;
; -------------------------------------------------------------------------- ;

; <FNC>           := <int number> <AND operation>
; <AND operation> := AND ( {<int number> | <OR operation>}+ )
; <OR operation>  := OR ( {<int number>}+ )

; -------------------------------------------------------------------------- ;
;                             LIST IMPLEMENTATION                            ;
; -------------------------------------------------------------------------- ;

; ------------------------------ CONSTRUCTORS ------------------------------ ;

; Contract: N, L -> L
; Purpose: returns the list-based representation of an FNC with a number of
; "var" variables and an AND "and-operation" that contains the the clauses.

(define fnc-list (
	lambda (vars and-operation) (
		list 'FNC vars and-operation
	)
))


; Contract: L -> L
; Purpose: returns the list representation of an AND that contains the clauses
; "clauses".

(define and-list (
	lambda (clauses) (
		list 'AND clauses
	)
))


; Contract: L -> L
; Purpose: returns the list representation of an OR that contains the vars
; "vars".

(define or-list (
	lambda (vars) (
		list 'OR vars
	)
))

; ------------------------------- PREDICATES ------------------------------- ;

; Contract: L -> B
; Purpose: returns "#t" if the expression "exp" is a fnc-list, "#f" otherwise.

(define fnc-list? (
	lambda (exp) (
		eqv? (car exp) 'FNC
	)
))

; Contract: L -> B
; Purpose: returns "#t" if the expression "exp" is a and-list, "#f" otherwise.

(define and-list? (
	lambda (exp) (
		eqv? (car exp) 'AND
	)
))

; Contract: L -> B
; Purpose: returns "#t" if the expression "exp" is a or-list, "#f" otherwise.

(define or-list? (
	lambda (exp) (
		eqv? (car exp) 'OR
	)
))

; ------------------------------- EXTRACTORS ------------------------------- ;

; Contract: L -> N
; Purpose: returns the number of vars in the fnc-list "fnc-list".

(define fnc-list->vars (
	lambda (fnc-list) (
		cadr fnc-list
	)
))

; Contract: L -> L
; Purpose: returns the and-operation in the fnc-list "fnc-list".

(define fnc-list->and-operation (
	lambda (fnc-list) (
		caddr fnc-list
	)
))

; Contract: L -> L
; Purpose: returns the clauses in the and-list "and-list".

(define and-list->clauses (
	lambda (and-list) (
		cadr and-list
	)
))

; Contract: L -> L
; Purpose: returns the vars in the or-list "or-list".

(define or-list->vars (
	lambda (or-list) (
		cadr or-list
	)
))

; ----------------------- EXAMPLES LIST IMPLEMENTATION --------------------- ;

; (X OR NOT y OR z OR w) AND (NOT x OR NOT y OR NOT z) AND w, x = 1, y = 2, z = 3 and w = 4

(define list-fnc-expression (fnc-list 4 (
		and-list (
			list (or-list '(1 -2 3 4)) (or-list '(-1 -2 -3)) 4
		)
	)
))

; (fnc-list->vars list-fnc-expression)
; (fnc-list->and-operation list-fnc-expression)
; (and-list->first-clause (fnc-list->and-operation list-fnc-expression))
; (and-list->rest-clauses (fnc-list->and-operation list-fnc-expression))
; (or-list->vars (and-list->first-clause (fnc-list->and-operation list-fnc-expression)))
; (or-list->first-var (and-list->first-clause (fnc-list->and-operation list-fnc-expression)))
; (or-list->rest-vars (and-list->first-clause (fnc-list->and-operation list-fnc-expression)))

; -------------------------------------------------------------------------- ;
;                          DATA TYPE IMPLEMENTATION                          ;
; -------------------------------------------------------------------------- ;

(define-datatype and_operation and_operation?
	(and-exp (clauses (list-of (
		lambda (element) (or (or_operation? element) (number? element))
	))))
)

(define-datatype or_operation or_operation?
	(or-exp (vars (list-of number?)))
)

(define-datatype fnc fnc?
	(fnc-exp (vars number?) (and-operation and_operation?))
)

; -------------------- EXAMPLE DATA TYPES IMPLEMENTATION ------------------- ;

; (X OR NOT y OR z OR w) AND (NOT x OR NOT y OR NOT z) AND w, x = 1, y = 2, z = 3 and w = 4

(define dataype-fnc-expression (fnc-list 4 (
		and-exp (
			list (or-exp '(1 -2 3 4)) (or-exp '(-1 -2 -3)) 4
		)
	)
))


(provide (all-defined-out))
