# vm_lisp

Load le code avec (load "vm.lisp")

Nous utilisons une variante du pseudo code lisp pour créer du code VM.
L'appel de fonction se fait avec le mot function (function test (2)).
Il faut toujours spécifier les arguments, même si il n'y en a pas (function test ()).
Le if doit être une structure comme suit : (if (*CONDITION*) (*CODE*) (*CODE*)).
Il faut toujours mettre des parenthèses même si la partie code contient qu'un entier ou qu'une variable.
(if (< 2 3) (10) (+ 2 3))

Création de la vm avec (vm_make). La vm à pour nom vm par défault.
Pour restaurer la vm à l'état initial on utilise clean_vm (clean_vm 'vm)

Pour compiler le pseudo code, utiliser la fonction compilation
Ex : (compilation '(defun test (n) (if (< n 3) (function test (+ n 1)) (n))))

Pour compiler le pseudo code et charger le code VM dans la vm utiliser vm_load
(vm_load 'vm '(defun test (n) (if (< n 3) (function test (+ n 1)) (n))))

Pour donner directement du code VM et le charger dans la vm utiliser vm_init_load
(vm_init_load 'vm '((MOVE (CONST 2) R0) (PUSH R0) (MOVE (CONST 3) R0) (PUSH R0) (POP R1) (POP R0) (ADD R1 R0)))

On éxécute la vm avec vm_run (vm_run 'vm)


Exemple d'utilisation :

(vm_make)
(vm_load 'vm '(defun fact (n) (if (<= n 1) (1) (* n (function fact (- n 1))))))
(vm_load 'vm '(function fact (9)))
(vm_run 'vm)
(clean_vm 'vm)
(vm_make)
*Mettre un autre code*

Fonction a test 

Marche
'(defun fact (n) (if (<= n 1) (1) (* n (function fact (- n 1))))) 
'(function fact (9))

code vm (utiliser vm_init_load 2 fois):

((@ FACT) (MOVE (REF -3) R0) (PUSH R0) (MOVE (CONST 1) R0) (PUSH R0) (POP R0) (POP R1) (CMP R1 R0) (MOVE (CONST 1) R1) (JLE (@ #:|else5066|)) (MOVE (CONST 0) R1) (@ #:|else5066|) (CMP R1 (CONST 0)) (JEQ (@ #:|sinon5064|)) (MOVE (CONST 1) R0) (JMP (@ #:|fin5065|)) (@ #:|sinon5064|) (MOVE (REF -3) R0) (PUSH R0) (MOVE (REF -3) R0) (PUSH R0) (MOVE (CONST 1) R0) (PUSH R0) (POP R1) (POP R0) (SUB R1 R0) (PUSH R0) (PUSH (CONST 1)) (JSR (@ FACT)) (PUSH R0) (POP R1) (POP R0) (MULT R1 R0) (@ #:|fin5065|) (RTN))

((MOVE (CONST 9) R0) (PUSH R0) (PUSH (CONST 1)) (JSR (@ FACT)))

Ne marche pas
'(defun fibo (n) (if (< n 2) (n) (+ (function fibo (- n 1)) (function fibo (- n 2)))))
'(function fibo (3))

Durand Cédric, Johann Golmard

