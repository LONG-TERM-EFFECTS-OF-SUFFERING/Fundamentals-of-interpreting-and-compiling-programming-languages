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

; Contract: L -> L
; Purpose: returns the list representation of an OR that contains the vars
; "vars".

(define or-list (
	lambda (vars) (
		list 'OR vars
	)
))

; (or-list '(1 2 3))
; (or-list '(4 5 6))
; (or-list '(7 8 9))


; Contract: L -> L
; Purpose: returns the list representation of an AND that contains the clauses
; "clauses".

(define and-list (
	lambda (clauses) (
		list 'AND clauses
	)
))

; (and-list '(1 2 3))
; (and-list (list 1 2 3 (or-list '(4 5 6))))
; (and-list (list (or-list '(1 2 3)) 4 5 6))


; Contract: N, L -> L
; Purpose: returns the list-based representation of an FNC with a number of
; "var" variables and an AND "and-operation" that contains the the clauses.

(define fnc-list (
	lambda (vars and-list) (
		list 'FNC vars and-list
	)
))

; (fnc-list 3 (and-list '(1 2 3)))
; (fnc-list 6 (and-list (list 1 2 3 (or-list '(4 5 6)))))
; (fnc-list 6 (and-list (list (or-list '(1 2 3)) 4 5 6)))

; ------------------------------- PREDICATES ------------------------------- ;

; Contract: L -> B
; Purpose: returns "#t" if the expression "exp" is a or-list, "#f" otherwise.

(define or-list? (
	lambda (exp) (
		eqv? (car exp) 'OR
	)
))

; (or-list? (or-list '(1 2 3)))
; (or-list? (or-list '(4 5 6)))
; (or-list? '(ORR (7 8 9)))


; Contract: L -> B
; Purpose: returns "#t" if the expression "exp" is a and-list, "#f" otherwise.

(define and-list? (
	lambda (exp) (
		eqv? (car exp) 'AND
	)
))

; (and-list? (and-list '(1 2 3)))
; (and-list? (and-list (list 1 2 3 (or-list '(4 5 6)))))
; (and-list? '(ANDD ((OR (1 2 3)) 4 5 6)))


; Contract: L -> B
; Purpose: returns "#t" if the expression "exp" is a fnc-list, "#f" otherwise.

(define fnc-list? (
	lambda (exp) (
		eqv? (car exp) 'FNC
	)
))

; (fnc-list? (fnc-list 3 (and-list '(1 2 3))))
; (fnc-list? (fnc-list 6 (and-list (list 1 2 3 (or-list '(4 5 6))))))
; (fnc-list? '(ABC 6 (AND ((OR (1 2 3)) 4 5 6))))

; ------------------------------- EXTRACTORS ------------------------------- ;

; Contract: L -> L
; Purpose: returns the vars in the or-list "or-list".

(define or-list->vars (
	lambda (exp) (
		if (or-list? exp)
			(cadr exp)
			(eopl:error "exp is not a or-list")
	)
))

; (or-list->vars (or-list '(1 2 3)))
; (or-list->vars (or-list '(4 5 6)))
; (or-list->vars '(ORR (7 8 9)))


; Contract: L -> L
; Purpose: returns the clauses in the and-list "and-list".

(define and-list->clauses (
	lambda (exp) (
		if (and-list? exp)
			(cadr exp)
			(eopl:error "exp is not an and-list")
	)
))

; (and-list->clauses (and-list '(1 2 3)))
; (and-list->clauses (and-list (list 1 2 3 (or-list '(4 5 6)))))
; (and-list->clauses '(ANDD ((OR (1 2 3)) 4 5 6)))


; Contract: L -> L
; Purpose: returns the and-operation in the fnc-list "fnc-list".

(define fnc-list->and-operation (
	lambda (exp) (
		if (fnc-list? exp)
			(caddr exp)
			(eopl:error "exp is not a fnc-list")
	)
))

; (fnc-list->and-operation (fnc-list 3 (and-list '(1 2 3))))
; (fnc-list->and-operation (fnc-list 6 (and-list (list 1 2 3 (or-list '(4 5 6))))))
; (fnc-list->and-operation '(FNCC 6 (AND ((OR (1 2 3)) 4 5 6))))


; Contract: L -> N
; Purpose: returns the number of vars in the fnc-list "fnc-list".

(define fnc-list->vars (
	lambda (exp) (
		if (fnc-list? exp)
			(cadr exp)
			(eopl:error "exp is not a fnc-list")
	)
))

; (fnc-list->vars (fnc-list 3 (and-list '(1 2 3))))
; (fnc-list->vars (fnc-list 6 (and-list (list 1 2 3 (or-list '(4 5 6))))))
; (fnc-list->vars '(FNCC 6 (AND ((OR (1 2 3)) 4 5 6))))

; ----------------------- EXAMPLES LIST IMPLEMENTATION --------------------- ;

; (x OR NOT y OR z OR w) AND (NOT x OR NOT y OR NOT z) AND w, x = 1, y = 2, z = 3 and w = 4

(define list-fnc-expression1 (fnc-list 4 (
		and-list (
			list (or-list '(1 -2 3 4)) (or-list '(-1 -2 -3)) 4
		)
	)
))

; a AND b AND c AND (d OR e OR f), a = 1, b = 2, c = 3, d = 4, e = 5 and f = 6

(define list-fnc-expression2 (fnc-list 4 (
		and-list (
			list 1 2 3 (or-list '(4 5 6))
		)
	)
))


; (a OR b OR c) AND d AND e AND f, a = 1, b = 2, c = 3, d = 4, e = 5 and f = 6

(define list-fnc-expression3 (fnc-list 4 (
		and-list (
			list (or-list '(1 -2 3 4)) (or-list '(-1 -2 -3)) 4
		)
	)
))


; list-fnc-expression1
; list-fnc-expression2
; list-fnc-expression3

; -------------------------------------------------------------------------- ;
;                          DATA TYPE IMPLEMENTATION                          ;
; -------------------------------------------------------------------------- ;

(define-datatype or_operation or_operation?
	(or-exp (vars (list-of number?)))
)

; (or-exp '(1 2 3))
; (or-exp '(4 5 6))
; (or-exp '(7 8 9))


(define-datatype and_operation and_operation?
	(and-exp (clauses (list-of (
		lambda (element) (
			or (or_operation? element) (number? element)
		)
	))))
)

; (and-exp '(1 2 3))
; (and-exp (list 1 2 3 (or-exp '(4 5 6))))
; (and-exp (list (or-exp '(1 2 3)) 4 5 6))


(define-datatype fnc fnc?
	(fnc-exp (vars number?) (and-operation and_operation?))
)

; (fnc-exp 3 (and-exp '(1 2 3)))
; (fnc-exp 6 (and-exp (list 1 2 3 (or-exp '(4 5 6)))))
; (fnc-exp 6 (and-exp (list (or-exp '(1 2 3)) 4 5 6)))

; -------------------- EXAMPLE DATA TYPES IMPLEMENTATION ------------------- ;

; (x OR NOT y OR z OR w) AND (NOT x OR NOT y OR NOT z) AND w, x = 1, y = 2, z = 3 and w = 4

(define dataype-fnc-expression1 (fnc-list 4 (
		and-exp (
			list (or-exp '(1 -2 3 4)) (or-exp '(-1 -2 -3)) 4
		)
	)
))


; a AND b AND c AND (d OR e OR f), a = 1, b = 2, c = 3, d = 4, e = 5 and f = 6

(define dataype-fnc-expression2 (fnc-list 4 (
		and-exp (
			list 1 2 3 (or-exp '(4 5 6))
		)
	)
))


; (a OR b OR c) AND d AND e AND f, a = 1, b = 2, c = 3, d = 4, e = 5 and f = 6

(define dataype-fnc-expression3 (fnc-list 4 (
		and-exp (
			list (or-exp '(1 -2 3 4)) (or-exp '(-1 -2 -3)) 4
		)
	)
))


; dataype-fnc-expression1
; dataype-fnc-expression2
; dataype-fnc-expression3


(provide (all-defined-out))
