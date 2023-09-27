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

(define fnc-list (
	lambda (vars and-operation) (
		list 'FNC vars and-operation
	)
))

(define and-list (
	lambda (clauses) (
		list 'AND clauses
	)
))

(define or-list (
	lambda (vars) (
		list 'OR vars
	)
))

; ------------------------------- PREDICATES ------------------------------- ;

(define fnc-list? (
	lambda (exp) (
		eqv? (car exp) 'FNC
	)
))

(define and-list? (
	lambda (exp) (
		eqv? (car exp) 'AND
	)
))

(define or-list? (
	lambda (exp) (
		eqv? (car exp) 'OR
	)
))

; ------------------------------- EXTRACTORS ------------------------------- ;

(define fnc-list->vars (
	lambda (fnc-list) (
		cadr fnc-list
	)
))

(define fnc-list->and-operation (
	lambda (fnc-list) (
		caddr fnc-list
	)
))

(define and-list->clauses (
	lambda (and-list) (
		cadr and-list
	)
))

(define and-list->first-clause (
	lambda (and-list) (
		car (and-list->clauses and-list)
	)
))

(define and-list->rest-clauses (
	lambda (and-list) (
		cdr (and-list->clauses and-list)
	)
))

(define or-list->vars (
	lambda (or-list) (
		cadr or-list
	)
))

(define or-list->first-var (
	lambda (or-list) (
		car (or-list->vars or-list)
	)
))

(define or-list->rest-vars (
	lambda (or-list) (
		cdr (or-list->vars or-list)
	)
))

; ----------------------- EXAMPLES LIST IMPLEMENTATION --------------------- ;

; (X OR NOT y OR z OR w) AND (NOT x OR NOT y OR NOT z) AND 4, x = 1, y = 2, z = 3 and w = 4

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

; (X OR NOT y OR z OR w) AND (NOT x OR NOT y OR NOT z) AND 4, x = 1, y = 2, z = 3 and w = 4

(define dataype-fnc-expression (fnc-list 4 (
		and-exp (
			list (or-exp '(1 -2 3 4)) (or-exp '(-1 -2 -3)) 4
		)
	)
))


(provide (all-defined-out))
