; Fonction a test 
; '(defun fact (n) (if (<= n 1) (1) (* n (function fact (- n 1)))))
; '(defun fibo (n) (if (< n 2) (n) (+ (function fibo (- n 1)) (function fibo (- n 2)))))
;

(defun compilation (exp &optional (argu_fonc ()))
    (cond
     ((member (car exp) '(+ - * /)) (compi-op exp argu_fonc))
     ((member (car exp) '(< > = <= >= )) (compi-compa exp argu_fonc))
     ((equal_cas exp 'if) (compi-if (cdr exp) argu_fonc))
	 ((equal_cas exp 'defun) (compi-defune (cdr exp)  argu_fonc))
     ((equal_cas exp 'function) (compi-fonction (cdr exp) argu_fonc))
     ((atom (car exp)) (compi-litt (car exp) argu_fonc))
     ((compi-fonction (cdar exp) argu_fonc)) ;pour les fonctions dans les opérations
    )
)


(defun compi-fonction(exp argu_fonc)
		 (let ((nb_argu (length (cdr exp))))
    	 (append (compi-param-fonc (cdr exp) argu_fonc)
    	 `((PUSH (CONST ,nb_argu)))	
         `((JSR (@ ,(car exp))))       
         )
         )    
)

(defun compi-param-fonc (exp argu_fonc)
 	(if (null (car exp))
		()
		(append (compilation (car exp) argu_fonc)
				`((PUSH R0))
				(compi-param-fonc (cdr exp) argu_fonc))
		)

)


(defun compi-defune (exp argu_fonc)
	;(write (cadr exp))
	(append `((@ ,(car exp))) ;;adresse de la function
			(compilation (car (cddr exp)) (cadr exp))
			'((RTN))
	)
)

(defun compi-if(exp argu_fonc)
	( let ((sinon (gensym "sinon"))(fin (gensym "fin")))
	(append (compilation (car exp) argu_fonc)
		'((CMP R1 (CONST 0))) ;; si ça n'a pas jump dans la compa
	    `((JEQ (@ ,sinon)))
		(compilation (cadr exp) argu_fonc)
		`((JMP (@ ,fin)))
	    `((@ ,sinon))  
	    (compilation (caddr exp) argu_fonc)
	    `((@ ,fin))
	)
	)
)

(defun compi-compa (exp argu_fonc)
	(let ((op (car exp))(else_ (gensym "else")))
	(append 
		;(format t "~% argu 1er ~S~%" (cadr exp))
  		;(format t "~% argu 2er ~S~%" (caddr exp))

  			(if (atom (cadr exp)) ; si c'est 2 ou (+ 2 3)
	         	(compilation  (list (cadr exp)) argu_fonc)
	         	(compilation  (cadr exp) argu_fonc)
        	)   

			'((PUSH R0))


			(if (atom (caddr exp)) ; si c'est 2 ou (+ 2 3)
	         	(compilation  (list (caddr exp)) argu_fonc )
	         	(compilation  (caddr exp) argu_fonc)
	        )
	   
			'((PUSH R0))
		    '((POP R0))
		    '((POP R1))
		    '((CMP R1 R0))
		    '((MOVE (CONST 1) R1)) ;; on met à T pour la future comparaison dans le if
		     (case op
		    	('= `((JEQ (@ ,else_))))
		    	('< `((JLT (@ ,else_))))
		    	('> `((JGT (@ ,else_))))
		    	('<= `((JLE (@ ,else_))))
		    	('>= `((JGE (@ ,else_))))
		    	('/= `((JNE (@ ,else_))))
		    )
		    '((MOVE (CONST 0) R1));;si ça n'a pas jump (voir le compi-if)
		    `((@ ,else_))
	)
	)
)

(defun compi-op (exp argu_fonc)
  (let ((operation (car exp))(argu (cdr exp)))
  		;(format t "~% argu 1er ~S~%" (car argu))
  		;(format t "~% argu 2er ~S~%" (cdr argu))
	(append 
   		(if (atom (car argu)) ; si c'est 2 ou (+ 2 3)
         	(compilation (list (car argu)) argu_fonc)
         	(compilation  (car argu) argu_fonc)
        )
		`((PUSH R0))
  
		(if (atom (cadr argu)) ; si c'est 2 ou (+ 2 3)
         	(compilation  (cdr argu) argu_fonc )
         	(compilation  (cadr argu) argu_fonc)
        )

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


(defun compi-litt(exp argu_fonc)
	(if (numberp  exp)
		`((MOVE (CONST ,exp) R0))
		(if (member exp argu_fonc)
			(compi-ref exp argu_fonc)
			(format t "Erreur : variable inconnue ~S~%" exp)
		)

	)
)

(defun compi-ref (exp argu_fonc)
	(let ((taille (list-length argu_fonc)))
		(setf real_taile (+ 3 taille)) ; ex : si 2 argument (x y) x mis en 1er dans la pile puis y puis le nombre d'argument. On a x dans la pile avec position de x dans la liste des arguments - (taille+1)
					 ; donc 1-4 = -3
		(setf pos (+ (position exp argu_fonc) 1))	; +1 car ça commence à 0
		(setf posi_dans_pile (- pos real_taile))
		`((MOVE (REF , posi_dans_pile) R0))
	)
)

(defun equal_cas (exp inst)
  (eql (car exp) inst)
  )
