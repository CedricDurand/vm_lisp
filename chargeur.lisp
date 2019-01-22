; Fonction a test 
; '(defun fact (n) (if (<= n 1) (1) (* n (function fact (- n 1)))))
; '(defun fibo (n) (if (< n 2) (n) (+ (function fibo (- n 1)) (function fibo (- n 2)))))
;

(defun compilation (exp)

    (cond
     ((member (car exp) '(+ - * /)) (compi-op exp ))
     ((member (car exp) '(< > = <= >= )) (compi-compa exp ))
     ((equal_cas exp 'if) (compi-if (cdr exp) ))
	 ((equal_cas exp 'defun) (compi-defune (cdr exp)  ))
     ((equal_cas exp 'function) (compi-fonction (cdr exp) ))
     ((atom (car exp)) (compi-litt (car exp) ))
     ((compi-fonction (cdar exp))) ;pour les fonctions dans les opérations
    )
)


(defun compi-fonction(exp)
  	(let ((nb_param (length (cdr exp))))
    	 (append (compi-param-fonc (cdr exp))
		 `((PUSH (CONST ,nb_param)))
		 `((MOVE BP R1))
		 `((MOVE SP BP))
		 `((MOVE SP R2))
		 `((SUB  (CONST ,nb_param) R2))
	     `((SUB  (CONST 1) :R2))
	     `((PUSH R2)) 
		 `((PUSH R1))
         `((JSR (@ ,(car exp))))       
         )        
    )
)

(defun compi-param-fonc (exp)
 	(if (atom exp)
		()
		(append (compilation (car exp))
				`((PUSH R0))
				(compi-param-fonc (cdr exp)))
		)

)


(defun compi-defune (exp)
	(append `((@ ,(car exp))) ;;adresse de la function
			(compilation (car (cddr exp))) ;; argu mis dans la pile
			'((RTN))
	)
)

(defun compi-if(exp)
	( let ((sinon (gensym "sinon"))(fin (gensym "fin")))
	(append (compilation (car exp))
		'((CMP R2 (CONST nil))) ;; si ça n'a pas jump dans la compa
	    `((JEQ (@ ,sinon)))
		(compilation (cadr exp))
		`((JMP (@ ,fin)))
	    `((@ ,sinon))  
	    (compilation (caddr exp))
	    `((@ ,fin))
	)
	)
)

(defun compi-compa (exp)
	(let ((op (car exp))(else_ (gensym "else")))
	(append (compilation (list (cadr exp)))
			'((PUSH R0))
			(compilation (list (caddr exp)))
			'((PUSH R0))
		    '((POP R0))
		    '((POP R1))
		    '((CMP R1 R0))
		    '((MOVE (CONST T) R2)) ;; on met à T pour la future comparaison dans le if
		     (case op
		    	('= `((JEQ (@,else_))))
		    	('< `((JLT (@,else_))))
		    	('> `((JGT (@,else_))))
		    	('<= `((JLE (@,else_))))
		    	('>= `((JGE (@,else_))))
		    	('/= `((JNE (@,else_))))
		    )
		    '((MOVE (CONST nil) R2));;si ça n'a pas jump (voir le compi-if)
		    `((@,else_))
	)
	)
)

(defun compi-op (exp)
  (let ((operation (car exp))(argu (cdr exp)))
	(append (compilation (list (car argu)) )
		`((PUSH R0))
		(compilation (cdr argu))
		`((PUSH R0))
		`((POP R1))
		`((POP R0))
		(case operation
		  ('+ `((ADD R1 R0)))
		  ('- `((SUB R1 R0)))
		  ('* `((MULT R1 R0)))
		  ('/ `((DIV R1 R0)))))     
	) 
 )


(defun compi-litt(exp)
	(if (numberp  exp)
		`((MOVE (CONST ,exp) R0))
		`((MOVE (REF , exp) R0))
	)
)

(defun equal_cas (exp inst)
  (eql (car exp) inst)
  )
