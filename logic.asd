(asdf:defsystem gui
  :components ((:module "logic"
			:serial t
			:components ((:file "package")
				     (:file "utils")
				     (:file "unify")))))