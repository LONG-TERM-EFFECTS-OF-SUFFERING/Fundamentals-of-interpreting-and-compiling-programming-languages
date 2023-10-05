; Calderón Prieto Brandon (2125874)
; Corrales Carlos Daniel (2122878)
; Melo Burbano Deisy (2041790)

#lang racket
(require (except-in eopl #%module-begin))
(provide (all-from-out eopl))
(provide #%module-begin)
(require "Exercise_1.rkt")
;;; (require "Exercise_2.rkt")

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

; Contract: L, L -> B
; Purpose: returns #t or #f if it satisfies for all possible permutations of truth values ​​of the variables
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

; Contract: L, L -> B
; Purpose: If all clauses are true, the function returns #t, #f otherwise
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

; Contract: L -> B
; Purpose: Evaluates all permutations one by one, if it finds at least one that satisfies the formula, 
; returns "satisfactory" along with that permutation. If no satisfactory permutation is found, returns "unsatisfactory"
(define EVALUARSAT (
  	lambda (exp)
    	(cond
      		((fnc-list? exp)
      			(let ((vars (fnc-list->vars  exp))
            		(bolean-operation (fnc-list->and-operation exp))
            		(proposals (all-proposals (fnc-list->vars exp))))
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
(EVALUARSAT prueba1)

(define prueba2 (fnc-list 2
  (and-list (list
    (or-list (list 1 2))
    (or-list (list -1))
    (or-list (list -2))
  ))
))
(EVALUARSAT prueba2)