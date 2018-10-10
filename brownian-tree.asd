(defsystem "brownian-tree"
  :version "0.1.0"
  :depends-on ("svg")
  :components ((:module "src"
                        :components ((:file "brownian-tree")
				     (:file "draw-brownian-tree")))))

(defsystem "brownian-tree/test"
  :depends-on ("brownian-tree"
	       "brownian-trails"
	       "svg"
	       "prove")
  :components ((:module "t"
			:serial t
			:components ((:file "util")
				     (:file "test-svg")
				     (:file "generate-brownian-trails")
				     (:file "test-brownian-tree")))))
