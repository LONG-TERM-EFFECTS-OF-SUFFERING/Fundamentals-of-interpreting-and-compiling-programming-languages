; Calderón Prieto Brandon (2125874)
; Corrales Carlos Daniel (2122878)
; Melo Burbano Deisy (2041790)

#lang racket
(require (except-in eopl #%module-begin))
(provide (all-from-out eopl))
(provide #%module-begin)
(require "Exercise_1.rkt")
;;; (require "Exercise_2.rkt")

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
; Purpose: returns "#t" if the expression "exp" is a or-list, "#f" otherwise.

(define or-list? (
	lambda (exp) (
		eqv? (car exp) 'OR
	)
))

; ------------------------------- EXTRACTORS ------------------------------- ;

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

; --------------------------- AUXILIAR FUNCTIONS --------------------------- ;

; Contract: transform-function (unary function), L -> L
; Purpose: returns a new list containing the results of applying the transform-function to
; each element of the input lists
(define nth-element (
	lambda (list n) (
		cond
			[(zero? n) (car list)]
			[else (nth-element (cdr list) (- n 1))]
	)
))

; Contract: L, n -> n
; Purpose: returns the element at position "n" in a given list, where "n" starts at zero.
(define (my-map transform list)
			(if (empty? list)
				'()
				(cons (transform (car list)) (my-map transform (cdr list)))
			)
)

; -----------------------Evaluacion de Instancias SAT----------------------- ;

; Contract: n(vars) -> L
; Purpose: returns all possible permutations of #t and #f that contains the "vars"
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

(define evaluate-exp (
	lambda (exp proposal) (
		letrec (
			(my-or (
				lambda (list) (
					cond
						[(empty? (cdr list)) (car list)]
						[else (or (car list) (my-or (cdr list)))]
				)
			))
		)
		(cond
			[(number? exp) (
				if (> exp 0)
					(nth-element proposal (- exp 1))
					(not (nth-element proposal (- (* -1 exp) 1)))
			)]
			[(or-list? exp) (my-or (my-map (
				lambda (exp) (
					evaluate-exp exp proposal
				)
			) (or-list->vars exp)))])
	)
))

(define evaluate-proposal (
	lambda (and-list proposal) (
		letrec (
			(evaluated-clauses (my-map (
				lambda (clause) (
					evaluate-exp clause proposal
				)
			) (and-list->clauses and-list)))
			(my-and (
				lambda (list) (
					cond
						[(empty? (cdr list)) (car list)]
						[else (and (car list) (my-and (cdr list)))]
				)
			))
		)
		(my-and evaluated-clauses)
	)
))

(define evaluate-sat (
  	lambda (exp)
    	(cond
      		((fnc-list? exp)
      			(let ((vars (cadr exp))
            		(bolean-operation (caddr exp))
            		(proposals (all-proposals (cadr exp))))
        				(letrec ((test-proposals
                  			(lambda (proposals)
                    			(cond
                       				[(empty? proposals) (format "unsatisfactory ~s" proposals)]
                       				[(evaluate-proposal bolean-operation (car proposals)) (format "satisfactory ~s" (car proposals))]
                       				[else (test-proposals (cdr proposals))]
                       			)
                   			)))
        		(test-proposals proposals)))
       		)
      	)
    )
)

(define prueba1 (fnc-list 4
  (and-list (list
    (or-list (list 1 -2 3 4))
    (or-list (list -2 3))
    (or-list (list -1 -2 -3))
    (or-list (list 3 4))
    (or-list (list 2))
  ))
))
(evaluate-sat prueba1)

(define prueba2 (fnc-list 2
  (and-list (list
    (or-list (list 1 2))
    (or-list (list -1))
    (or-list (list -2))
  ))
))
(evaluate-sat prueba2)