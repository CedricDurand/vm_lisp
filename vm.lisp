(require "chargeur.lisp")
(defun vm_make (&optional (taille 5000) (name 'VM))
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
    (setf (get name 'LABEL) (make-hash-table :size 0))
    ; pour l'arret 
    (setf (get name 'RUN) nil)
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
    (setf (get name 'RUN) nil)
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
	    ;('VARG (case-varg mv exp inst))
	    ;('JSR (case-saut mv exp inst))
	    ;('FEntry (case-fonction mv exp inst))
	    ;(otherwise (format t "Instruction : ~S~%" inst))
	    )
	      ;faire quelque chose avec 'memory, afin de mettre le code assembleur en mémoire que run executera
	  do (setf (aref (get vm 'memory) (get vm 'SP)) inst)
	  do (exec_incr vm 'SP)
	  do (setf exp (cdr exp))
	  do (setf inst (car exp))
	  )
    )
    (write (get vm 'memory))
    (get vm 'LABEL)
)

(defun register_function (vm exp)
	(setf (gethash (cdr exp) (get vm 'LABEL)) (get vm 'SP))
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
(defun vm_get_register (vm reg)
	(get vm reg) 
)

(defun vm_set_register (vm reg val)
  	(setf (get vm reg) val) 
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
  (vm_set_register vm reg (vm_get_memory vm adr))
)

(defun exec_store (vm reg adr)
  (vm_set_memory vm adr (vm_get_register vm reg))
)

(defun exec_push (vm reg)
  (exec_store vm reg (- (vm_get_memory vm (get vm 'SP)) 1 ))
  (vm_set_memory vm (- (get vm 'SP) 1))
)

(defun exec_pop (vm reg)
  (exec_load vm (vm_get_memory vm (get vm 'SP)) reg)
  (vm_set_memory vm (+ (get vm 'SP) 1))
)

; reg 1 ici peut être une valeur faire vérification
(defun exec_add (vm reg1 reg2)
  (vm_set_register vm reg2 (+ (vm_get_register vm reg1) (vm_get_register vm reg2)))   
)

(defun exec_sub (vm reg1 reg2)  
   (vm_set_register vm reg2 (- (vm_get_register vm reg1) (vm_get_register vm reg2))) 
)

(defun exec_mult (vm reg1 reg2)  
   (vm_set_register vm reg2 (/ (vm_get_register vm reg1) (vm_get_register vm reg2))) 
)

(defun exec_div (vm reg1 reg2)
   (vm_set_register vm reg2 (* (vm_get_register vm reg1) (vm_get_register vm reg2))) 
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

;(defun exec_cmp (vm reg1 reg2) ;à faire
;  	(let ((var1 (vm_get_register vm reg1)) (var2 (vm_get_register vm reg2)))
;		(cond
;	   		((= var1 var2) (vm_set_register vm 'FEQ 1) (vm_set_register vm 'FGT 0) (vm_set_register vm 'FLT 0))
;	     	((> var1 var2) (vm_set_register vm 'FGT 1) (vm_set_register vm 'FEQ 0) (vm_set_register vm 'FLT 0))
;	      	((< var1 var2) (vm_set_register vm 'FLT 1) (vm_set_register vm 'FEQ 0) (vm_set_register vm 'FGT 0))   
;	   	))
;)
(defun exec_cmp (vm reg1 reg2)
  (let ((r1 (vm_get_register vm reg1)) (r2 (vm_get_register vm reg1)))
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
  (setf (compare (bit-and #*100  my-array))
	(if (not (equal compare #*000))
    	(vm_set_register name 'SP lbl))))

(defun exec_jle (vm lbl)
  (setf my-array (make-array 3 :element-type 'bit))
  (setf (aref my-array 0) (vm_get_register vm 'FLT))
  (setf (aref my-array 1) (vm_get_register vm 'FEQ))
  (setf (aref my-array 2) (vm_get_register vm 'FGT))
  (setf (compare (bit-and #*110  my-array))
  (if (not (equal compare #*000))
      (vm_set_register name 'SP lbl))))

(defun exec_jgt (vm lbl)
  (setf my-array (make-array 3 :element-type 'bit))
  (setf (aref my-array 0) (vm_get_register vm 'FLT))
  (setf (aref my-array 1) (vm_get_register vm 'FEQ))
  (setf (aref my-array 2) (vm_get_register vm 'FGT))
  (setf (compare (bit-and #*001  my-array))
  (if (not (equal compare #*000))
      (vm_set_register name 'SP lbl))))

(defun exec_jge (vm lbl)
  (setf my-array (make-array 3 :element-type 'bit))
  (setf (aref my-array 0) (vm_get_register vm 'FLT))
  (setf (aref my-array 1) (vm_get_register vm 'FEQ))
  (setf (aref my-array 2) (vm_get_register vm 'FGT))
  (setf (compare (bit-and #*011  my-array))
  (if (not (equal compare #*000))
      (vm_set_register name 'SP lbl))))

(defun exec_jeq (vm lbl)
  (setf my-array (make-array 3 :element-type 'bit))
  (setf (aref my-array 0) (vm_get_register vm 'FLT))
  (setf (aref my-array 1) (vm_get_register vm 'FEQ))
  (setf (aref my-array 2) (vm_get_register vm 'FGT))
  (setf (compare (bit-and #*010  my-array))
  (if (not (equal compare #*000))
      (vm_set_register name 'SP lbl))))

(defun exec_jne (vm lbl)
  (setf my-array (make-array 3 :element-type 'bit))
  (setf (aref my-array 0) (vm_get_register vm 'FLT))
  (setf (aref my-array 1) (vm_get_register vm 'FEQ))
  (setf (aref my-array 2) (vm_get_register vm 'FGT))
  (setf (compare (bit-and #*101  my-array))
  (if (not (equal compare #*000))
      (vm_set_register name 'SP lbl))))

(defun exec_nop (vm))

(defun exec_halt (vm)
	(vm_set_etat vm nil))


;changer les fonctions pour que ça corresponde 
(defun vm_eval (vm expr)
	(case (car expr)
       	(MOVE  (exec_move vm (cdr expr))) 
    	(STORE (exec_store vm (cdr expr)))
	    (LOAD (exec_load vm (cdr expr)))
     	(ADD (exec_add vm (cdr expr)))
      	(SUB (exec_sub vm (cdr expr)))
        (MUL (exec_mult vm (cdr expr)))
        (DIV (exec_div vm (cdr expr)))
        (INCR (exec_incr vm (cadr expr)))
        (DECR (exec_decr vm (cadr expr)))
        (PUSH (exec_push vm (cadr expr)))
        (POP (exec_pop vm (cadr expr)))
        (CMP (exec_cmp vm (cadr expr)))
        (JMP (exec_jmp vm (cadr expr)))
        (JSR (exec_jsr vm (cadr expr)))
        (RTN (exec_rtn vm (cdr expr)))
        (JGT (exec_jgt vm (cadr expr)))
        (JGE (exec_jge vm (cadr expr)))
        (JLT (exec_jlt vm (cadr expr)))
        (JLE (exec_jle vm (cadr expr)))
        (JEQ (exec_jeq vm (cadr expr)))
        (JNE (exec_jne vm (cadr expr)))
        (NOP (exec_nop vm (cadr expr)))
        (HALT (exec_halt vm (cdr expr)))
    )  
)

(defun vm_run ())


(defun existe_register (expression)
  (member expression '(R0 R1 R2 PC BP SP FLT FEQ FGT )))

(defun existe_jumpp (expression)
  (member expression '(JMP JSR JEQ JNE JGT JLT JGE JLE)))
