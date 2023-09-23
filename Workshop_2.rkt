; ---------------------------- 1.1 Gramática BNF --------------------------- ;

; <FNC>             := <number> <logic operation>
; <logic operation> := OR (<logic operation>  <logic operation>)
; <logic operation> := AND (<logic operation>  <logic operation>)
; <logic operation> := <boolean value>


; -------------------------------------------------------------------------- ;
;                             LIST IMPLEMENTATION                            ;
; -------------------------------------------------------------------------- ;

; ------------------------------ CONSTRUCTORS ------------------------------ ;

(define fnc-list (
	lambda (vars logic_operation) (
		list 'FNC vars logic_operation
	)
))

(define or-list (
	lambda (rand1 rand2) (
		list 'OR rand1 rand2
	)
))

(define and-list (
	lambda (rand1 rand2) (
		list 'AND rand1 rand2
	)
))

; ------------------------------- EXTRACTORS ------------------------------- ;

(define fnc->vars (
	lambda (fnc-list) (
		cadr fnc-list
	)
))

(define or-list->rand1 (
	lambda (or-list) (
		cadr or-list
	)
))

(define or-list->rand2 (
	lambda (or-list) (
		caddr or-list
	)
))

(define and-list->rand1 (
	lambda (and-list) (
		cadr and-list
	)
))

(define and-list->rand2 (
	lambda (and-list) (
		caddr and-list
	)
))


; -------------------------------- EXEMPLES -------------------------------- ;


; (X OR NOT y OR z OR w) AND (NOT x OR NOT y OR NOT z) x = 1, y = 2, z = 3 and w = 4

(define expression1 (fnc-list 4
	(
		and-list
			(or-list (or-list (or-list 1 -2) 3) 4)
			(or-list (or-list -1 -2) -3)
	)
))


; -------------------------------------------------------------------------- ;
;                          DATA TYPES IMPLEMENTATION                         ;
; -------------------------------------------------------------------------- ;
