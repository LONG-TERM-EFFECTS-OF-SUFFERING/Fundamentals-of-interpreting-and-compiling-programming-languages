; Calderón Prieto Brandon (2125874)
; Corrales Carlos Daniel (2122878)
; Melo Burbano Deisy (2041790)

#lang eopl

; -------------------------------------------------------------------------- ;
;                             AUXILIAR FUNCTIONS                             ;
; -------------------------------------------------------------------------- ;

(define nth-element (
	lambda (list n) (
		cond
			[(zero? n) (car list)]
			[else (nth-element (cdr list) (- n 1))]
	)
))

; -------------------------------------------------------------------------- ;
;                              1. SAT INSTANCES                              ;
; -------------------------------------------------------------------------- ;

; ---------------------------- 1.1 Gramática BNF --------------------------- ;

; <FNC>             := <number> <logic operation>
; <logic operation> := OR (<logic operation>  <logic operation>)
; <logic operation> := AND (<logic operation>  <logic operation>)
; <logic operation> := <boolean var>


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

(define boolean_var-list (
	lambda (number) (
		list 'BOOLEAN number
	)
))

; ------------------------------- PREDICATES ------------------------------- ;

(define fnc-list? (
	lambda (exp) (
		eqv? (car exp) 'FNC
	)
))

(define or-list? (
	lambda (exp) (
		eqv? (car exp) 'OR
	)
))

(define and-list? (
	lambda (exp) (
		eqv? (car exp) 'AND
	)
))

(define boolean_var-list? (
	lambda (exp) (
		eqv? (car exp) 'BOOLEAN
	)
))

; ------------------------------- EXTRACTORS ------------------------------- ;

(define fnc-list->vars (
	lambda (fnc-list) (
		cadr fnc-list
	)
))

(define fnc-list->logic-operation (
	lambda (fnc-list) (
		caddr fnc-list
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

(define boolean_var-list->number (
	lambda (boolean_var-list) (
		cadr boolean_var-list
	)
))

; ----------------------- EXAMPLES LIST IMPLEMENTATION --------------------- ;

; (X OR NOT y OR z OR w) AND (NOT x OR NOT y OR NOT z) x = 1, y = 2, z = 3 and w = 4

(define list-fnc-expression (fnc-list 4
	(
		and-list
			(or-list (or-list (or-list (boolean_var-list 1) (boolean_var-list -2)) (boolean_var-list 3)) (boolean_var-list 4))
			(or-list (or-list (boolean_var-list -1) (boolean_var-list -2)) (boolean_var-list -3))
	)
))

; -------------------------------------------------------------------------- ;
;                          DATA TYPE IMPLEMENTATION                          ;
; -------------------------------------------------------------------------- ;

(define-datatype logic_operation logic_operation?
	(boolean-var-exp (value number?))
	(or-exp (rand1 logic_operation?) (rand2 logic_operation?))
	(and-exp (rand1 logic_operation?) (rand2 logic_operation?))
)

(define-datatype fnc fnc?
	(fnc-exp (vars number?) (logic-operation logic_operation?))
)

; -------------------- EXAMPLE DATA TYPES IMPLEMENTATION ------------------- ;

; (X OR NOT y OR z OR w) AND (NOT x OR NOT y OR NOT z) x = 1, y = 2, z = 3 and w = 4

(define dataype-fnc-expression (fnc-exp 5
	(
		and-exp
			(or-exp (or-exp (or-exp (boolean-var-exp 1) (boolean-var-exp -2)) (boolean-var-exp 3)) (boolean-var-exp 4))
			(or-exp (or-exp (boolean-var-exp -1) (boolean-var-exp -2)) (boolean-var-exp -3))
	)
))

; -------------------------------------------------------------------------- ;
;                        2. PARSE Y UNPARSE FUNCTIONS                        ;
; -------------------------------------------------------------------------- ;

; ----------------------------- LOGIC OPERATION ---------------------------- ;

(define unparse-logic_operation-expression (
	lambda (exp) (
		cases logic_operation exp
			(boolean-var-exp (value) (boolean_var-list value))
			(or-exp (rand1 rand2) (or-list
				(unparse-logic_operation-expression rand1)
				(unparse-logic_operation-expression rand2)))
			(and-exp (rand1 rand2) (and-list
				(unparse-logic_operation-expression rand1)
				(unparse-logic_operation-expression rand2)))
	)
))


(define parse-logic_operation-expresion (
	lambda (exp) (
		cond
			[(boolean_var-list? exp) (boolean-var-exp (boolean_var-list->number exp))]
			[(or-list? exp)
				(or-exp (parse-logic_operation-expresion (or-list->rand1 exp))
						(parse-logic_operation-expresion (or-list->rand2 exp)))]
			[(and-list? exp)
				(and-exp (parse-logic_operation-expresion (and-list->rand1 exp))
						(parse-logic_operation-expresion (and-list->rand2 exp)))]
	)
))

; ----------------------------------- FNC ---------------------------------- ;

(define unparse-fnc-expression (
	lambda (exp) (
		cases fnc exp
			(fnc-exp (vars bolean-operation) (fnc-list vars (unparse-logic_operation-expression bolean-operation)))
	)
))

(define parse-fnc-expresion (
	lambda (exp) (
		cond
			[(fnc-list? exp) (fnc-exp
				(fnc-list->vars exp)
				(parse-logic_operation-expresion (fnc-list->logic-operation exp)))]
	)
))


; (unparse-fnc-expression dataype-fnc-expression)
; (parse-fnc-expresion (unparse-fnc-expression dataype-fnc-expression))

; -------------------------------------------------------------------------- ;
;                       3. EVALUATION OF SAT INSTANCES                       ;
; -------------------------------------------------------------------------- ;

(define evaluate-proposal (
	lambda (logic-operation proposal) (
		cases logic_operation logic-operation
			(boolean-var-exp (value) (
				if (> value 0)
					(nth-element proposal (- value 1))
					(not (nth-element proposal (- (* -1 value) 1)))
			))
			(or-exp (rand1 rand2) (or
				(evaluate-proposal rand1 proposal)
				(evaluate-proposal rand2 proposal)))
			(and-exp (rand1 rand2) (and
				(evaluate-proposal rand1 proposal)
				(evaluate-proposal rand2 proposal)))
	)
))

(define all-proposals (
	lambda (vars) (
		letrec (
			(generate-all-proposals (
				lambda (vars proposals) (
					letrec (
						(generate-new-permutation (
							lambda (list1 list2) (
								letrec (
										(insert-in-all (
											lambda (element list) (
												if (null? list)
													empty
													(cons (cons element (car list)) (insert-in-all element (cdr list)))
											)
										))
									)
									(if (null? list1)
										empty
										(append (insert-in-all (car list1) list2) (generate-new-permutation (cdr list1) list2)))
							)
						))
					)
					(cond
						[(= vars 1) proposals]
						[else (generate-all-proposals (- vars 1) (generate-new-permutation '(#t #f) proposals))])
				)
			))
		)
		(generate-all-proposals vars '((#t) (#f)))
	)
))

(define evaluate-sta (
	lambda (exp) (
		cases fnc exp
			(fnc-exp (vars bolean-operation) (
				letrec (
					(test-proposals (
						lambda (bolean-operation proposals) (
							cond
								[(empty? proposals) (format "unsatisfactory ~s" proposals)]
								[(evaluate-proposal bolean-operation (car proposals)) (format "satisfactory ~s" (car proposals))]
								[else (test-proposals bolean-operation (cdr proposals))]
						)
					))
				)
				(test-proposals bolean-operation (all-proposals vars))
			))
	)
))


; ------------------------- EXAMPLES STA EVALUATION ------------------------ ;

; FNC 4 ((1 OR -2 OR 3 OR 4) AND (-2 OR 3) AND (-1 OR -2 OR -3) AND (3 OR 4) AND 2)

(define test1 (fnc-exp 4
	(
		and-exp
			(and-exp
				(and-exp
					(or-exp (or-exp (or-exp (boolean-var-exp 1) (boolean-var-exp -2)) (boolean-var-exp 3)) (boolean-var-exp 4))
					(or-exp (boolean-var-exp -2) (boolean-var-exp 3)))
				(and-exp
					(or-exp (or-exp (boolean-var-exp -1) (boolean-var-exp -2)) (boolean-var-exp -3))
					(or-exp (boolean-var-exp 3) (boolean-var-exp 4))))
			(boolean-var-exp 2)
	)
))

; FNC 2 ((1 OR 2) AND -1 and -2)

(define test2 (fnc-exp 2
	(
		and-exp
			(and-exp
				(or-exp (boolean-var-exp 1) (boolean-var-exp 2))
				(boolean-var-exp -1))
			(boolean-var-exp -2)
	)
))

(evaluate-sta test1)
(evaluate-sta test2)
