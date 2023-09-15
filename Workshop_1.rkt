#lang racket
; Calderón Prieto Brandon (2125874)
; Corrales Carlos Daniel (2122878)
; Melo Burbano Deisy (2041790)

; --------------------------- AUXILIAR FUNCTIONS --------------------------- ;

; Contract: L, L -> L
; Purpose: returns the concatenation of "list1" and "list2".
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
; Purpose: returns "list", but adding the element "element" in the last position of the list.
; <list> := ()
;        := (<scheme value> <list>)

(define add (
	lambda (element list) (
		cond
			[(empty? list) (cons element empty)]
			[else (cons (car list) (add element (cdr list)))]
	)
))

; Contract: transform-function (unary function), L -> L
; Purpose: returns a new list containing the results of applying the transform-function to
; each element of the input list
; <list> := ()
;        := (<scheme value> <list>)

(define (mapAux transform list)
			(if (empty? list)
				'()
				(cons (transform (car list)) (mapAux transform (cdr list)))
			)
)

; Contract: L1, L2 -> L
; Purpose: returns a list of pairs, where each pair consists of element L1 and an element from list L2.
; <list> := ()
;        := (<scheme value> <list>)

(define cartesian-productAux
  	(lambda (l1 l2)
		(if (empty? l2)
			'()
			(cons (list l1 (car l2)) (cartesian-productAux l1 (cdr l2)))
		)
	)
)

; -------------------------------------------------------------------------- ;


; Exercise 1
; Contract: L -> L
; Purpose: returns "list", but with its inverted ordered pairs.
; <list> := ()
;        := (<scheme value> <list>)

(define invert (
	lambda (list) (
		let (
			; Contract: L -> L
			; Purpose: returns "pair", but with its inverted elements.
			; <list> := ()
			;        := (<scheme value> <list>)

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
; (invert '((1 2) (3 4) (5 6)))


; Exercise 2
; Contract: L -> L
; Purpose: returns a list with each element of "list" associated with one more level of parentheses
; compared to its original state in "list".
; <list> := ()
;        := (<scheme value> <list>)

(define down (
	lambda (list) (
		let (
			; Contract: E -> L
			; Purpose: returns "element", but in a list.
			; <list> := ()
			;        := (<scheme value> <list>)

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
; (down '(((1) (2)) ((3) (4)) ((5) (6))))


; Exercise 3
; Contract: L, N, X -> L
; Purpose: returns "list", but with element "x" at position "n".
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
; (list-set '((1 2) (3 4)) 1 '(5 6))


; Exercise 4
; Contract: P, L -> L
; Purpose: returns a list that contains the elements that belong to "list" and satisfy the predicate "p".
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


(define list-index
	(lambda(p list)
		(letrec((search (lambda(index list)
							(cond
								[(empty? list) #f]
								[(p (car list)) index]
								[else (search (+ index 1) (cdr list))]
								))
					))
		(search 0 list)
			)
		)
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

(define swapper
	(lambda (e1 e2 list)
    	(letrec ((swap (lambda (x)
                	(cond
                    	[(equal? x e1) e2]
                    	[(equal? x e2) e1]
                    	[else x]
						))
					))
    (mapAux swap list))))

; (swapper 'a 'd '(a b c d)) -> (d b c a)
; (swapper 'a 'd '(a d () c d)) -> (d a () c a)
; (swapper 'x 'y '(y y x y x y x x y)) -> (x x y x y x y y x)

; Exercise 7
; Contract: L1, L2 -> L
; Purpose: Returns a list of tuples with the Cartesian product between L1 and L2
; <list> := ()
;        := (<scheme value> <list>)

(define cartesian-product
	(lambda(l1 l2)
		(if (empty? l1)
			'()
			(concat (cartesian-productAux (car l1) l2) (cartesian-product (cdr l1) l2))
		)
	)
)

; (cartesian-product '(a b c) '(x y)) -> ((a x) (a y) (b x) (b y) (c x) (c y))
; (cartesian-product '(p q r) '(5 6 7)) -> ((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))


; Exercise 9
; Contract: L -> n
; Purpose: Returns the number of inversions 'n' in the list 'L'
; <lista> := ()
;         := (<number> <lista>)

(define inversions (lambda (l)
		    (letrec(
			(compair(lambda (l n)
                           (if  (null? (cdr l))
                                0
                           (if (> n (cadr l))
                               (+ 1 (compair(cdr l) n) )
                               (compair(cdr l) n)
                               )))
		    ))
                    (if  (null? (cdr l))
                         0
                        (+ (compair l (car l) ) (inversions (cdr l)))
                         )

                     )))

 ;Proofs
; (inversions'(2 3 8 6 1))
; (inversions'(1 2 3 4))
; (inversions'(3 2 1))


;Exercise 10
; Contract: L -> L'
; Purpose: Receive a list and remove a pair of parentheses from each element within the list
; if the element doesn't have parentheses, then include it in the resulting output without any modification.
; <list> := ()
;        := (<scheme-value> <list>)

(define up(lambda (list)
		(letrec(
			(add(lambda (listA listB)
					(if (null? listA)
							listB
							(cons (car listA) (add (cdr listA) listB))
							))
				))
			(cond
			[(null? list) empty]
			[(list? (car list)) (add (car list) (up (cdr list)))]
			[else (cons (car list) (up (cdr list)))]
			)
			)
		)
)

;Proofs
; (up '((1 2) (3 4)))
; (up '((x (y)) z))
; (up '((1 2 3) 4 ((e (f)))))


; Exercise 11
; Contract:F, L1, L2 -> L
; Purpose: Return a list where the n position corresponds to the result of
; applying function F to the elements at the nth position in L1 and L2.
;<int - list> ::= ()
;             ::= (<int> <int - list>)

(define zip (lambda (f L1 L2)
	(if (null? L1)
		empty
		(cons
			(f (car L1) (car L2))
			(zip f (cdr L1) (cdr L2)))
	)
))

;Proofs
; (zip + '(1 4) '(6 2))
; (zip * '(11 5 6) '(10 9 8))
; (zip - '(2 3) '(5 4))


; Exercise 12
; Contract: a, b, F, acum, filter -> n
; Propose: Apply the binary function F to all elements within
; the interval [a, b] that also satisfy the predicate of function F.

(define filter-acum(lambda (a b F acum filter)
	(if (> a b)
		acum
		(if (filter a)
			(filter-acum (+ a 1) b F (F a acum) filter)
			(filter-acum (+ a 1) b F acum filter)
			))
))

; Proofs
; (filter-acum 1 10 + 0 odd?)
; (filter-acum 1 10 + 0 even?)
; (filter-acum 1 10 + 1 number?)

; Exercise 13
; Contract: lrators, lrands -> n
; Propose: Return the result of successively applying the operations
; in 'lrators' to the values in 'lrands'.
; <list> := (<int> <list>)

(define operate
  (lambda (Irators Irands)(
    letrec(
        (explore(
           lambda (l)(
             if (null? l) 0 (+ 1 (explore(cdr l))))))
      )
     (if (< (explore Irands) (explore Irators)) 'error
        (if (null? (cdr Irators))
        ((car Irators) (car Irands) (cadr Irands))
        ( operate (cdr Irators) (cons ((car Irators) (car Irands) (cadr Irands)) (cddr Irands)))))
  )))


;Proofs
;(operate (list + * + - *) '(1 2 8 4 11 6))
;(operate (list *) '(4 5))
;(operate (list + - *) '(0 1 2 3 4 5))


; Exercise 14
; Contract: n, bst -> list
; Propose: Return a list with the path to take, as indicated by a binary tree, to reach the received number n.
; If the number is in the root node, return an empty list.
; <binaryTree> := ( ́emptyTree) empty
;                 := (nodo) number <binaryTree> < binaryTree>


(define path (lambda (n bst)
                    
  (cond
    [(null? bst) empty] 
    [(= n (car bst)) '()] 
    [(< n (car bst)) (cons 'left (path n (cadr bst)))]
    [else (cons 'right (path n (caddr bst)))]
  )))

; Proofs
;(path 17 '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
;(path 14 '(14 () ()))
;(path 7 '(8 (3 () (1 () (6 (4 () ()) (7 () ())))) (10 () (14 (13 () ()) ())) ))
;(path 13 '(8 (3 () (1 () (6 (4 () ()) (7 () ())))) (10 () (14 (13 () ()) ())) ))


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
; (Operar-binarias '( (5 multiplica 3) multiplica (6 multiplica 7)))


; Exercise 17
; Contract: mat, vec -> List
; Purpose: Return the result of performing matrix-vector multiplication
; <matrix> := ()
;          := (<int-list> <matrix>)
;
; <int-list> := ()
;            := (<int> <int-list>)

(define prod-scalar-matrix
  (lambda (mat vec)
    (letrec(
          (prod-scalar-vector
            (lambda (vec1 aux)
              (cond
                [(or (null? vec1) (null? aux)) empty]
                [else (cons (* (car vec1) (car aux)) (prod-scalar-vector (cdr vec1) (cdr aux)))]
              )))
        )
      (if (null? mat)
          empty
          (cons (prod-scalar-vector (car mat) vec) (prod-scalar-matrix (cdr mat) vec))
      ))
))

;Proofs
;(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
;(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
;(prod-scalar-matriz '((2 2) (1 3) (6 4)) '(3 3))

; Exercise 18
; Contract: N -> L
; Purpose: returns the row "N" of Pascal's triangle.
; <list> := ()
;        := (<scheme value> <list>)

(define pascal (
	lambda (n) (
		letrec (
			; Contract: N, L -> L
			; Purpose: returns the row "N" of Pascal's triangle.
			; <list> := ()
			;        := (<scheme value> <list>)

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
; (pascal 4)
