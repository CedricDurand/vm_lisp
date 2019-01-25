(require "chargeur.lisp")
(defun vm_make (&optional (taille 500) (name 'VM))
  	; Notre mémoire
 	(setf (get name 'memory) (make-array taille :initial-element ()))
  	; Nos 7 registres
   	(setf (get name 'R0) 0)
  	(setf (get name 'R1) 0)
  	(setf (get name 'R2) 0)
     	; registre qui pointe sur la prochaine instruction (compteur ordinal)
  	(setf (get name 'PC) 0)
   	; registre qui pointe sur le bas de la pile
   	(setf (get name 'BP) 0)
  	; registre qui pointe sur le haut de la pile (case vide)
   	(setf (get name 'SP) 0) 
    ; Les 3 flags booléens
    (setf (get name 'FLT) 0)
    (setf (get name 'FEQ) 0)
    (setf (get name 'FGT) 0)
    ; les etiquettes qui seront dans une hastable
    (setf (get name 'LABEL) (make-hash-table :size 0 :test 'equalp))
    ; pour l'arret 
    (setf (get name 'RUN) 1)
)


(defun clean_vm (name)
  (prog1
    (setf (get name 'R0) 0)
    (setf (get name 'R1) 0)
    (setf (get name 'R2) 0)
    (setf (get name 'PC) 0)
    (setf (get name 'BP) 0)
    (setf (get name 'SP) 0)
    (setf (get name 'FLT) 0)
    (setf (get name 'FEQ) 0)
    (setf (get name 'FGT) 0)
    (setf (get name 'LABEL) (make-hash-table :size 0))
    (setf (get name 'RUN) 1)
    (loop for posi from 0 to (- (length (get name 'memory)) 1)
      do
		(set_memory name posi () ))))

(defun vm_load (vm code)
  (let ((liste_expression (compilation code)))
    (vm_init_load vm liste_expression) 
  )
)

(defun vm_init_load (vm liste_expression)

    (let ((exp liste_expression)
	(inst (car liste_expression)))
    	(loop while exp
    	do
	  (case (car inst)
	    ('@ (register_function vm inst))
	   )
	  do (setf (aref (get vm 'memory) (get vm 'SP)) inst)
	  do (exec_incr vm 'SP)
	  do (setf exp (cdr exp))
	  do (setf inst (car exp))
	  )
    )
    (get vm 'memory)
    ;(maphash #'(lambda (clé val) (format t "~a => ~a~%" clé val)) (get vm 'LABEL))
)

(defun register_function (vm exp)
	(setf (gethash (symbol-name (cadr exp)) (get vm 'LABEL)) (get vm 'SP))
	(setf (aref (get vm 'memory) (get vm 'SP)) exp)
	(exec_incr vm 'SP)	
)

(defun vm_load_file (vm file)
  
)

(defun get_memory (vm posi)
  (aref (get vm 'memory) posi)
)

(defun set_memory (vm  posi valeur)
	(setf (aref (get vm 'memory) posi) valeur)
)

; faire l'initialisation

(defun vm_init(name)
  (vm_load_file name "ASM_loader"))



; accésseur 
(defun vm_set_etat ( vm val )
	(setf (get vm 'RUN) val))

(defun get_etat ( vm )
	(get vm 'RUN))


;voir ici si on doit faire une vérification
;(defun vm_get_register (vm reg)
;	(get vm reg) 
;)
(defun vm_get_register (vm adr)
	(cond
		((not (listp adr)) (get vm adr))
    ((existe_constante adr) (cadr adr))
		((existe_variable  adr) (cadr adr))
		((existe_adresse  adr) (cadr adr))
		;((get vm adr))
	)
)

(defun get-hash (key)
  (gethash key (get 'vm 'LABEL))  
)

(defun vm_set_register (vm reg val)
  ;(format t "~% VAL hash : ~S~%"  val)
    (if (atom val)
        (setf (get vm reg) val)
        ;(format t "~% LABEL hash : ~S~%" (getvaleur 'else7055 (get vm 'LABEL))) 
        (setf (get vm reg) (get-hash (symbol-name (cadr val))))
    )
    ;(format t "~% PC hash : ~S~%"  (get vm 'PC))
)

(defun vm_get_memory (vm adr)
  	(aref (get vm 'memory) adr)
)

(defun vm_set_memory (vm adr val)
    (setf (aref (get vm 'memory) adr) val)
)

(defun exec_move (vm reg1 reg2)
  (vm_set_register vm reg2 (vm_get_register vm reg1))
)


(defun exec_load (vm adr reg)
;(format t "~% EXEC LOAD : ~S~%" adr)
  (vm_set_register vm reg (vm_get_memory vm adr))
)

(defun exec_store (vm reg adr)
  (vm_set_memory vm adr (vm_get_register vm reg))
)

(defun exec_push (vm reg)
  (exec_store vm reg (get vm 'SP))
  (exec_incr vm 'SP)
)

(defun exec_pop (vm reg)
;(format t "~% POP ICI : ~S~%" (get vm 'SP))
  (exec_decr vm 'SP)
  (exec_load vm (get vm 'SP) reg)
)

; reg 1 ici peut être une valeur faire vérification
(defun exec_add (vm reg1 reg2)
  (vm_set_register vm reg2 (+ (vm_get_register vm reg1) (vm_get_register vm reg2)))   
)

(defun exec_sub (vm reg1 reg2)  
   (vm_set_register vm reg2 (- (vm_get_register vm reg2) (vm_get_register vm reg1))) 
)

(defun exec_mult (vm reg1 reg2)  
   (vm_set_register vm reg2 (* (vm_get_register vm reg1) (vm_get_register vm reg2))) 
)

(defun exec_div (vm reg1 reg2)
   (vm_set_register vm reg2 (/ (vm_get_register vm reg2) (vm_get_register vm reg1))) 
)

(defun exec_decr (vm reg) 
   (vm_set_register vm reg (- (vm_get_register vm reg) 1))  
)

(defun exec_incr (vm reg) 
   (vm_set_register vm reg (+ (vm_get_register vm reg) 1)) 
)


; pour l'instant on met directement l'adresse après test si c'est un label ou une adresse
; à voir si on fait une table d'association plus tard en variable locale de la vm
(defun exec_jmp (vm lbl)  
	(vm_set_register vm 'PC lbl)
)

(defun exec_jsr (vm lbl)  
	(exec_push vm (exec_incr 'PC))
 	(exec_jmp vm lbl)
)

(defun exec_rtn (vm) 
	(exec_load vm (get vm 'SP) 'R0)
 	(exec_decr vm 'SP)
  	(exec_jmp vm 'R0)	  
)

(defun exec_cmp (vm reg1 reg2)
  (let ((r1 (vm_get_register vm reg1)) (r2 (vm_get_register vm reg2)))
  (cond
    ((eql r1 r2) (vm_set_register vm 'FEQ 1) (vm_set_register vm 'FLT 0) (vm_set_register vm 'FGT 0))
    ((> r1 r2) (vm_set_register vm 'FGT 1) (vm_set_register vm 'FLT 0) (vm_set_register vm 'FEQ 0))
    ((< r1 r2) (vm_set_register vm 'FLT 1) (vm_set_register vm 'FGT 0) (vm_set_register vm 'FEQ 0))
    ))  
  )

(defun exec_jlt (vm lbl)
  (setf my-array (make-array 3 :element-type 'bit))
  (setf (aref my-array 0) (vm_get_register vm 'FLT))
  (setf (aref my-array 1) (vm_get_register vm 'FEQ))
  (setf (aref my-array 2) (vm_get_register vm 'FGT))
  (setf compare (bit-and #*100  my-array))
	(if (not (equal compare #*000))
    	(vm_set_register vm 'PC lbl)))

(defun exec_jle (vm lbl)
  (setf my-array (make-array 3 :element-type 'bit))
  (setf (aref my-array 0) (vm_get_register vm 'FLT))
  (setf (aref my-array 1) (vm_get_register vm 'FEQ))
  (setf (aref my-array 2) (vm_get_register vm 'FGT))
  (setf compare (bit-and #*110  my-array))
  (if (not (equal compare #*000))
      (vm_set_register vm 'PC lbl)))

(defun exec_jgt (vm lbl)
  (setf my-array (make-array 3 :element-type 'bit))
  (setf (aref my-array 0) (vm_get_register vm 'FLT))
  (setf (aref my-array 1) (vm_get_register vm 'FEQ))
  (setf (aref my-array 2) (vm_get_register vm 'FGT))
  (setf compare (bit-and #*001  my-array))
  (if (not (equal compare #*000))
      (vm_set_register vm 'PC lbl)))

(defun exec_jge (vm lbl)
  (setf my-array (make-array 3 :element-type 'bit))
  (setf (aref my-array 0) (vm_get_register vm 'FLT))
  (setf (aref my-array 1) (vm_get_register vm 'FEQ))
  (setf (aref my-array 2) (vm_get_register vm 'FGT))
  (setf compare (bit-and #*011  my-array))
  (if (not (equal compare #*000))
      (vm_set_register vm 'PC lbl)))

(defun exec_jeq (vm lbl)
  (setf my-array (make-array 3 :element-type 'bit))
  (setf (aref my-array 0) (vm_get_register vm 'FLT))
  (setf (aref my-array 1) (vm_get_register vm 'FEQ))
  (setf (aref my-array 2) (vm_get_register vm 'FGT))
  (setf compare (bit-and #*010  my-array))
  (if (not (equal comp #*000))
      (vm_set_register vm 'PC lbl)))

(defun exec_jne (vm lbl)
  (setf my-array (make-array 3 :element-type 'bit))
  (setf (aref my-array 0) (vm_get_register vm 'FLT))
  (setf (aref my-array 1) (vm_get_register vm 'FEQ))
  (setf (aref my-array 2) (vm_get_register vm 'FGT))
  (setf compare (bit-and #*101  my-array))
  (if (not (equal compare #*000))
      (vm_set_register vm 'PC lbl)))

(defun exec_nop (vm))

(defun exec_halt (vm)
	(vm_set_etat vm nil)
  ;(format t "~% HALT : ~S~%" (get vm 'RUN))
 )


;changer les fonctions pour que ça corresponde 
(defun vm_eval (vm expr)

	(case (car expr)
       	(MOVE  (exec_move vm (cadr expr) (caddr expr))) 
       	(ADD (exec_add vm (cadr expr) (caddr expr)))
      	(SUB (exec_sub vm (cadr expr) (caddr expr)))
        (MULT (exec_mult vm (cadr expr) (caddr expr)))
        (DIV (exec_div vm (cadr expr) (caddr expr)))
        (PUSH (exec_push vm (cadr expr)))
        (POP (exec_pop vm (cadr expr)))
        (CMP (exec_cmp vm (cadr expr) (caddr expr)))
        (JMP (exec_jmp vm (cadr expr)))
        (JSR (exec_jsr vm (cadr expr)))
        (RTN (exec_rtn vm ))
        (JGT (exec_jgt vm (cadr expr)))
        (JGE (exec_jge vm (cadr expr)))
        (JLT (exec_jlt vm (cadr expr)))
        (JLE (exec_jle vm (cadr expr)))
        (JEQ (exec_jeq vm (cadr expr)))
        (JNE (exec_jne vm (cadr expr)))
        (NOP (exec_nop vm ))
        (HALT (exec_halt vm))
        ('@ ;(format t "Label défini ~%")
            )
        (otherwise (format t "Erreur : instruction inconnue ~%"))
    )  
)

(defun vm_run (vm)
	;(write (get vm 'memory))
	(setf (get vm 'PC) 0)
	(loop while (and (get vm 'PC) (not(eq (get vm 'PC) (get vm 'SP))))
		do(if (aref (get vm 'memory) (get vm 'PC))
		(progn
			;(format t "~% Registre RO ~S~%" (get vm 'R0))
			;(format t "~% Registre R1 ~S~%" (get vm 'R1))
      (format t "~% en cours  ~S~%" (aref (get vm 'memory) (get vm 'PC)))
			(vm_eval vm (aref (get vm 'memory) (get vm 'PC)))
			(exec_incr vm 'PC)
		)
		;(get vm 'LABEL)
	  )
	)
	(get vm 'R0)
	;(cond
	;	((existe_register (car (cddr (aref (get vm 'memory) 2)))) (format t "LA bite en bois ~%") (format t "LA bite en bois ~%"))
	;	(t 	(format t "Turlututu chapo pointu ~%"))	
	;)
)


(defun existe_constante (expe)
 (format t "~% Constante ~S~%" expe)
	(if (eq (car expe) 'CONST) 1 nil)
)
(defun existe_variable (expe)
	(if (eq (car expe) 'REF) 1 nil)
)
(defun existe_adresse (exp)
	(if (eq (car exp) '@) 1 nil)
)
(defun existe_register (expression)
  (member expression '(R0 R1 R2 PC BP SP FLT FEQ FGT )))

(defun existe_jumpp (expression)
  (member expression '(JMP JSR JEQ JNE JGT JLT JGE JLE)))
