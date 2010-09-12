(in-package :logic)
(defconstant fail nil)
(defconstant no-bindings (if (constantp 'no-bindings)
			     no-bindings
			     '((t . t))))

(defun variable-p (x)
  (and (symbolp x)
       (equal (char (symbol-name x) 0) #\?)))

#+nil
(variable-p '?x)

(defun get-binding (var bindings)
  (assoc var bindings))

#+nil
(get-binding 'blub '((blub . 2)))

(defun binding-val (binding)
  (cdr binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

#+nil
(lookup 'blub '((blub . 2)))

(defun extend-bindings (var val bindings)
  (cons (cons var val)
	(if (eq bindings no-bindings)
	    nil
	    bindings)))

#+nil
(extend-bindings 
 'blub 2 
 (extend-bindings 'bla 3 no-bindings))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t fail))))

#+nil
(match-variable 'blub 2 '((blub . 2)))


(defun unify (x y &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
	((eql x y) bindings)
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
	((and (consp x) (consp y))
	 (unify (rest x) (rest y)
		(unify (first x) (first y) bindings)))
	(t fail)))

(defparameter *occurs-check* t)

(defun occurs-check (var x bindings)
  (cond ((eq var x) t)
	((and (variable-p x) (get-binding x bindings))
	 (occurs-check var (lookup x bindings) bindings))
	((consp x) (or (occurs-check var (first x) bindings)
		       (occurs-check var (rest x) bindings)))
	(t nil)))

(defun unify-variable (var x bindings)
  (cond ((get-binding var bindings)
	 (unify (lookup var bindings) x bindings))
	((and (variable-p x) (get-binding x bindings))
	 (unify var (lookup x bindings) bindings))
	((and *occurs-check* (occurs-check var x bindings))
	 fail)
	(t (extend-bindings var x bindings))))

#+nil
(list
 (unify '(?x + 1) '(2 + ?y))
 (unify '?x '?y)
 (unify '(?x ?x) '(?y ?y))
 (unify '(?x ?x ?x) '(?y ?y ?y))
 (unify '(?x ?y ?a) '(?y ?x ?x))
 (unify '?x '(f ?x)))

(defun subst-bindings (bindings x)
  (cond ((eq bindings fail) fail)
	((eq bindings no-bindings) x)
	((and (variable-p x) (get-binding x bindings))
	 (subst-bindings bindings (lookup x bindings)))
	((atom x) x)
	(t (reuse-cons (subst-bindings bindings (car x))
		       (subst-bindings bindings (cdr x))
		       x))))

(defun unifier (x y)
  (subst-bindings (unify x y) x))

#+nil
(unifier '(?x ?y a) '(?y ?x ?x))