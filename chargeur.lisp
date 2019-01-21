(defun compilation (exp)
	;(write exp)
    (cond
     ((member (car exp) '(+ - * /)) (compi-op exp ))
     ((member (car exp) '(< > = <= >= )) (compi-compa exp ))
     ((equal_cas exp 'if) (compi-if (cdr exp) ))
	 ((equal_cas exp 'defun) (compi-defune (cdr exp)  ))
     ((equal_cas exp 'function) (compi-fonction (cdr exp) ))
     ((atom (car exp)) (compi-litt (car exp) ))
    )
)


(defun compi-fonction(exp)
  	(let ((nb_param (length (car (cdr exp)))))
		(write nb_param)
    	 
     
     
    )
;	(append (compi-param-fonc (cdr exp))
;		`((PUSH (CONST nb_param)))
;		`((MOVE ))
;		`((MOVE ))
;		`((MOVE ))
		
		;;pas fini
;	)

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
