;; incremental interpreter p. 383
(in-package :logic)


(defun prove-all (goal bindings)
  (cond ((eq bindings fail) fail)
	((null goals) bindings)
	(t (prove (first goals)
		  bindings
		  (rest goals)))))

(defun prove (goal bindings)
  (some #'(lambda (clause)
	    (let ((new-clause (rename-variables clause)))
	      (prove-all (append (clause-body goal) other-goals))
	      (unify goal (clause-head new-clause) bindings)))
	(get-clauses (predicate goal))))