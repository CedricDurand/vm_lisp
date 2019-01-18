
(defun compilation (exp)
    (cond
     ((member (caar exp) '(+ - * /)) (compi-op exp ))
     ;;((member (car exp) '(< > = <= >= )) (compi-compa exp ))
     ;;((equal_cas exp 'if) (compi-if exp ))
	;;equal_cas exp 'defun) (compi-defune exp  ))
    ;; (`(function ,(car exp)) (compi-fonction exp ))
     ;;((atom (cdr exp)) (compi-litt (caar exp) ))
    )
)


(defun compi-fonction(exp)
	(let ((nb_param (length (cdr exp)))))
	(append (compi-param-fonc (cdr exp))
		`((PUSH (:CONST nb_param)))
		`((MOVE ))
		`((MOVE ))
		`((MOVE ))
		
		;;pas fini
	)

)

(defun compi-param-fonc (exp)
	(if (atom exp)
		()
		(append (compilation (car exp))
				`((PUSH :R0))
				(compi-param-fonc (cdr exp)))
		)

)

(defun compi-litt(exp)
	(if (numberp  exp)
		`((MOVE (:CONST ,exp) :R0))
		`((MOVE (:REF , exp) :R0))
	)
)



(defun compi-defune (exp)
	(append '((ENTRY))
			`((@ ,(cadr exp))) ;;@ pour adresse
			(compilation (cddr exp)) ;; argu mis dans la pile
			'((RTN))
			'((QUIT))
	)
)


(defun compi_compa (exp)
	(let ((op (car exp))))
	(append (compilation (cadr exp))
			'((PUSH :R0))
			(compilation (caddr exp))
			'((PUSH :R0))
		    '((POP :R0))
		    '((POP :R1))
		    '((CMP :R1 :R0)) 
		    (case op
		    	('= `((JEQ (@,(gensym "testfin")))))
		    	('< `((JLT (@,(gensym "testfin")))))
		    	('> `((JGT (@,(gensym "testfin")))))
		    	('<= `((JLE (@,(gensym "testfin")))))
		    	('>= `((JGE (@,(gensym "testfin")))))
		    	('/= `((JNE (@,(gensym "testfin")))))
		    )
	)
)

(defun compi-if(exp) 
	(append (compilation (car exp))
		`((CMP :R0 ))

	)
)

(defun compi-op (exp)
  (let ((operation (caar exp))(argu (caadr exp)))
  (write (car exp))
  
	;(append (compilation (car argu) )
	;	`((PUSH :R0))
	;	(compilation (cadr argu))
	;	`((PUSH :R0))
	;	`((POP :R1))
	;	`((POP :R0))
	;	(case operation
	;	  ('+ `((ADD :R1 :R0)))
	;	  ('- `((SUB :R1 :R0)))
	;	  ('* `((MULT :R1 :R0)))
	;	  ('/ `((DIV :R1 :R0)))))     
	) 
 )

(defun equal_cas (exp inst)
  (eql (car exp) inst)
  )
