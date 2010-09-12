(asdf:defsystem logic
  :components ((:module "logic"
			:serial t
			:components ((:file "package")
				     (:file "utils")
				     (:file "unify")
				     (:file "prolog0")))))
