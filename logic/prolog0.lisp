(in-package :logic)

(defun clause-head (clause) (car clause))
(defun clause-body (clause) (cdr clause))
(defun get-clauses (predicate) (get predicate 'clauses))
(defun predicate (relation) (car relation))

(defvar *db-predicates* nil)

(defmacro <- (&rest clause)
  `(add-clause ',clause))

(defun add-clause (clause)
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred)
		 (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
	  (nconc (get-clauses pred) (list clause)))
    pred))

(defun clear-predicate (predicate)
  (setf (get predicate 'clauses) nil))

(defun clear-db ()
  (mapc #'clear-predicate *db-predicates*))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  (if (atom tree)
      (if (funcall predicate tree)
	  (adjoin tree found-so-far)
	  found-so-far)
      (unique-find-anywhere-if 
       predicate (first tree)
       (unique-find-anywhere-if predicate (rest tree) found-so-far))))

(defun variables-in (exp)
  (unique-find-anywhere-if #'variable-p exp))

#+nil
(variables-in '((?x + 2 ?y)))

(defun rename-variables (x)
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
		  (variables-in x))
	  x))

#+nil
(rename-variables '((?x + 2 ?y)))

(defun prove-all (goals bindings)
  (cond ((eq bindings fail) fail)
	((null goals) (list bindings))
	(t (mapcan #'(lambda (goal1-solution)
		       (prove-all (rest goals) goal1-solution))
		   (prove (first goals) bindings)))))

(defun prove (goal bindings)
  (mapcan #'(lambda (clause)
	      (let ((new-clause (rename-variables clause)))
		(prove-all (clause-body new-clause)
			   (unify goal (clause-head new-clause)
				  bindings))))
	  (get-clauses (predicate goal))))

(defmacro ?- (&rest goals)
  `(prove-all ',goals no-bindings))

#+nil
(progn
  (<- (likes Kim Robin))
  (<- (likes Sandy Lee))
  (<- (likes Sandy Kim))
  (<- (likes Robin cats))
  (<- (likes Sandy ?x) (likes ?x cats))
  (<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
  (<- (likes ?x ?x))
  (?- (likes Sandy ?who)))