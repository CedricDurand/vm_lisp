(defun code_to_asm (expr)
	(cond (...)


		  (t (//STOP la compilation))))


(defun compilation (exp)
  (let ((arg (if (atom exp) () (cdr exp))))
    (cond
     ((atom exp) (compi-litt exp ))
     ((member (car exp) '(+ - * /)) (compi-op exp ))
     ((member (car exp) '(< > = <= >= )) (compi-compa exp ))
     ((equal_cas exp 'if) (compi-if exp ))
     ((equal_cas exp 'defun) (compi-defun arg  ))
     (`(function ,(car exp)) (compi-appel exp ))
    )
  )
)

(defun compi-defun )

(defun compi_compa (exp)
	(let ((op (car exp))))
	(append (compilation (cadr exp))
			'((PUSH R0))
			(compilation (caddr exp))
			'((PUSH R0))
		    '((POP R0))
		    '((POP R1))
		    '((CMP R1 R0)) 
		    (case op
		    	('= `((JEQ mettrre un truc ici)))
		    	('< `((JLT mettrre un truc ici)))
		    	('> `((JGT mettrre un truc ici)))
		    	('<= `((JLE mettrre un truc ici)))
		    	('>= `((JGE mettrre un truc ici)))
		    	('/= `((JNE mettrre un truc ici)))
		    )
	)
)

(defun compi-if(exp) 
	(append (compilation (car exp))
		'((CMP R0 ))

	)

)

(defun compi-op (exp)
  (let ((operation (car exp))(argu (cdr exp)))

	(append (compilation (car argu) )
		'((PUSH R0))
		(compilation (cadr argu))
		'((PUSH R0))
		'((POP R1))
		'((POP R0))
		(case operation
		  ('+ '((ADD R1 R0)))
		  ('- '((SUB R1 R0)))
		  ('* '((MULT R1 R0)))
		  ('/ '((DIV R1 R0)))))      
    )
 )

(defun compi_litt(exp)
	'((MOVE (car exp) R0))
)

(defun equal_cas (exp inst)
  (eql (car exp) inst)
  )
