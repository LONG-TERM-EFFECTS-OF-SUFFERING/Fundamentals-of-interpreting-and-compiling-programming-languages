#lang racket
(require (except-in eopl #%module-begin))
(provide (all-from-out eopl))
(provide #%module-begin)
; Calderón Prieto Brandon (2125874)
; Corrales Carlos Daniel (2122878)
; Melo Burbano Deisy (2041790)


; -------------------------------------------------------------------------- ;
;                                   GRAMMAR                                  ;
; -------------------------------------------------------------------------- ;

; <programa>          ::= <expression>
;                          <un-programa (exp)>

; <expresion>        ::= <numero>
;                         numero-lit (num)

;                     ::= "\""<texto>"\""
;                         texto-lit (txt)

;                     ::= <identificador>
;                         var-exp (id)

;                     ::= (<expresion> <primitiva-binaria> <expresion>)
;                         primapp-bin-exp (exp1 prim-binaria exp2)

;                     ::= <primitiva-unaria> (<expresion>)
;                         primapp-un-exp (prim-unaria exp)

;                     ::= Si <expresion> entonces <expresion> sino <expresion> finSI
;                         <condicional-exp (test-exp true-exp false-exp)>

;                     ::= declarar ( {<identificador>=<expresion>}*(;) ) {<expresion>}
;                         variableLocal-exp (ids exps cuerpo)

;                     ::= procedimiento ( {<identificador>}*(,) ) haga <expresion> finProc
;                         procedimiento-exp (ids cuerpo)

;                     ::= evaluar <expresion> ( {expresion}*(,) ) finEval
;                         app-exp (rator rands)

;                     ::= letrec {<identificador> ( {<identificador}*(,) ) = <expresion>}* {<expresion>}
;                         letrec-exp (proc-names idss bodies letrec-body)
;
; <primitiva-binaria> ::= + (primitiva-suma)
;                     ::= ~ (primitiva-resta)
;                     ::= / (primitiva-div)
;                     ::= * (primitiva-multi)
;                     ::= concat (primitiva-concat)
;
; <primitiva-unaria>  ::= longitud (primitiva-longitud)
;                     ::= add1 (primitiva-add1)
;                     ::= sub1 (primitiva-sub1)

; -------------------------------------------------------------------------- ;

(define lexica '(
	(white-sp (whitespace) skip)
	(comment ("//" (arbno (not #\newline))) skip)
	(identifier ("@" letter (arbno (or letter digit "?"))) symbol)
	(text (letter (arbno (or letter digit "?" ":"))) string)
	(number (digit (arbno digit)) number)
	(number ("-" digit (arbno digit)) number)
	(number ( digit (arbno digit) "." digit (arbno digit)) number)
        (number ( "-" digit (arbno digit) "." digit (arbno digit)) number)
))


(define grammar '(
		(programa (expresion) un-programa)
		(expresion (number) number-lit)
		(expresion ("\"" text "\"") texto-lit)
		(expresion (identifier) var-exp)
		(expresion ("(" expresion primitiva-binaria expresion ")" ) primapp-bin-exp)
		(expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)
		(expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp)
		(expresion ("declarar" "(" (separated-list identifier "=" expresion ";") ")" "{" expresion "}") variableLocal-exp)
		(expresion ("procedimiento" "(" (separated-list identifier ",") ")" "haga" expresion "finProc") procedimiento-exp)
		(expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)
		(expresion ("letrec" "{"
			(arbno identifier "(" (separated-list identifier ",") ")" "=" expresion) "}" expresion)
			letrec-exp)
		(primitiva-binaria ("+") primitiva-suma)
		(primitiva-binaria ("~") primitiva-resta)
		(primitiva-binaria ("/") primitiva-div)
		(primitiva-binaria ("*") primitiva-multi)
		(primitiva-binaria ("concat") primitiva-concat)
		(primitiva-unaria ("longitud") primitiva-longitud)
		(primitiva-unaria ("add1") primitiva-add1)
		(primitiva-unaria ("sub1") primitiva-sub1)
))

; -------------------------------------------------------------------------- ;
;                                   SLLGEN                                   ;
; -------------------------------------------------------------------------- ;

(sllgen:make-define-datatypes lexica grammar)


(define show-the-datatypes (
	sllgen:list-define-datatypes lexica grammar
))


(define scan&parse (
	sllgen:make-string-parser lexica grammar
))


(define just-scan (
	sllgen:make-string-scanner lexica grammar
))

(define interpretador (sllgen:make-rep-loop "-->"
                                            (lambda (pgm) (eval-program pgm))
                                            (sllgen:make-stream-parser
                                              lexica
                                              grammar)))
; -------------------------------------------------------------------------- ;
;                                ENVIRONMENTS                                ;
; -------------------------------------------------------------------------- ;

; -------------------------------- AUXILIAR -------------------------------- ;

(define nth-element (
	lambda (list n) (
		cond
			[(zero? n) (car list)]
			[else (nth-element (cdr list) (- n 1))]
	)
))

(define index-of (
	lambda (element list) (
		letrec (
			(get-index (
				lambda (element list index) (
					cond
						[(empty? list) #f]
						[(eqv? (car list) element) index]
						[else (get-index element (cdr list) (+ index 1))]
				)
			))
		)
		(get-index element list 0)
	)
))

; -------------------------------------------------------------------------- ;

(define-datatype environment environment?
	(empty-environment)
	(extended-environment
		(symbols (list-of symbol?))
		(values (list-of scheme-value?))
		(env environment?))
	(recusively-extended-environment
		(procedures-names (list-of symbol?))
		(procedures-parametes (list-of (list-of scheme-value?)))
		(bodies (list-of expresion?))
		(env environment?))
)


(define scheme-value? (lambda (anything) #t))


(define buscar-variable (
	lambda (env var) (
		cases environment env
			(empty-environment () (eopl:error "Error, la variable no existe"))
			(extended-environment (symbols values old-env) (
					let (
						(index (index-of var symbols))
					)
					(if index
						(nth-element values index)
						(buscar-variable old-env var)
					)
				)
			)
			(recusively-extended-environment (procedures-names procedures-parameters procedures-bodies old-env) (
				let (
					(index (index-of var procedures-names))
				)
				(if index
					(cerradura
						(nth-element procedures-parameters index)
						(nth-element procedures-bodies index)
						env
					)
					(buscar-variable old-env var)
				)
			))
	)
))

; -------------------------------------------------------------------------- ;
;                              PROGRAM EVALUATOR                             ;
; -------------------------------------------------------------------------- ;

(define init-env (
	lambda () (
		extended-environment '(@a @b @c @d @e) '(1 2 3 "hola" "FLP") (empty-environment)
	)
))


(define-datatype procVal procVal? (
	cerradura
		(identifiers (list-of symbol?))
		(body expresion?)
		(env environment?)

))


(define apply-procedure (
	lambda (proc arguments) (
		cases procVal proc
			(cerradura (identifiers body env)
				(eval-expression body (extended-environment identifiers arguments env)))
	)
))


(define valor-verdad? (
	lambda (value) (
		not (zero? value)
	)
))


(define eval-program (
	lambda (pgm) (
		cases programa pgm
			(un-programa (body) (eval-expression body (init-env)))
	)
))


(define eval-expressions (
	lambda (expressions env) (
		map (lambda (expression) (eval-expression expression env)) expressions
	)
))


(define eval-expression (
	lambda (exp env) (
		cases expresion exp
			(number-lit (number) number)
			(texto-lit (text) text)
			(var-exp (identifier) (buscar-variable env identifier))
			(primapp-bin-exp (rand1 rator rand2) (
					let (
						(evaluated-rand1 (eval-expression rand1 env))
						(evaluated-rand2 (eval-expression rand2 env))
					)
					(apply-binary-primitive evaluated-rand1 rator evaluated-rand2)
				)
			)
			(primapp-un-exp (rator rand) (
					let (
						(evaluated-rand (eval-expression rand env))
					)
					(apply-unary-primitive rator evaluated-rand)
				)
			)
			(condicional-exp (test-exp true-exp false-exp)
				(if (valor-verdad? (eval-expression test-exp env))
					(eval-expression true-exp env)
					(eval-expression false-exp env)
				)
			)
			(variableLocal-exp (identifiers expressions body) (
				let (
					(args (eval-expressions expressions env))
				)
				(eval-expression body (extended-environment identifiers args env))
			))
			(procedimiento-exp (identifiers body) (cerradura identifiers body env))
			(app-exp (rator rands) (
				let (
					(proc (eval-expression rator env))
					(args (eval-expressions rands env))
				)
				(if (procVal? proc)
					(apply-procedure proc args)
					(eopl:error "exp ~s is not a procedure" proc)
				)
			))
			(letrec-exp (procedures-names procedures-parameters procedures-bodies body) (
				eval-expression body (recusively-extended-environment
					procedures-names
					procedures-parameters
					procedures-bodies
					env
				)
			))
		)
))


(define apply-unary-primitive (
	lambda (rator rand) (
		cases primitiva-unaria rator
			(primitiva-longitud () (string-length rand))
			(primitiva-add1 () (+ rand 1))
			(primitiva-sub1 () (- rand 1))
	)
))


(define apply-binary-primitive (
	lambda (rand1 rator rand2) (
		cases primitiva-binaria rator
			(primitiva-suma () (+ rand1 rand2))
			(primitiva-resta () (- rand1 rand2))
			(primitiva-div () (/ rand1 rand2))
			(primitiva-multi () (* rand1 rand2))
			(primitiva-concat () (string-append rand1 rand2))
	)
))

; -------------------------------------------------------------------------- ;
;                                   TESTING                                  ;
; -------------------------------------------------------------------------- ;

(eval-program (scan&parse "
	// Si (2 + 3) entonces 2 sino 3 finSI
	// Si (longitud(@d) ~ 4) entonces 2 sino 3 finSI

	// declarar (@x = 2; @y = 3; @a = 7) {
	// 	(@a + (@x ~ @y))
	// }

	// declarar (@x = 2; @y = 3; @a = 7) {
	// 	(@a + @b)
	// }

	// declarar (
	// 	@x = 2;
	// 	@y = 3;
	// 	@a = procedimiento (@x, @y, @z) haga ((@x + @y) + @z) finProc
	// ) {
	// 	evaluar @a (1 , 2, @x) finEval
	// }

	// declarar (
	// 	@x = procedimiento (@a, @b) haga ((@a * @a) + (@b * @b)) finProc;
	// 	@y = procedimiento (@x, @y) haga (@x + @y) finProc
	// ) {
	// 	(evaluar @x (1,2) finEval + evaluar @y (2,3) finEval)
	// }

	// declarar (
	// 	@x = Si (@a * @b) entonces (@d concat @e) sino longitud((@d concat @e)) finSI;
	// 	@y = procedimiento (@x, @y) haga (@x + @y) finProc
	// ) {
	// 	(longitud (@x) * evaluar @y (2,3) finEval)
	// }
"))

; -------------------------------------------------------------------------- ;
;                                  QUESTIONS                                 ;
; -------------------------------------------------------------------------- ;


; ------------------------------------ 1 ----------------------------------- ;
