(in-package :asdf-user)
(defvar +proj-version+ "0.1.0")

(defsystem "svg"
  :version +proj-version+
  
  :components ((:module "src"
			:serial t
			:components ((:file "svg/object")
				     (:file "svg/text")
				     (:file "svg/circle")
				     (:file "svg/ellipse")
				     (:file "svg/rect")
				     (:file "svg/polygon")
				     (:file "svg/path")
				     (:file "svg")
				     (:file "color")))))
(defsystem "brownian-tree"
  :version +proj-version+
  :depends-on ("svg")
  :components ((:module "src"
                        :components ((:file "brownian-tree")))))

(defsystem "brownian-trails"
  :version +proj-version+
  :depends-on ("svg")
  :components ((:module "src"
			:components ((:file "brownian-trails")))))

(defsystem "brownian-tree/test"
  :depends-on ("brownian-tree"
	       "brownian-trails"
	       "svg"
	       "prove")
  :components ((:module "t"
			:components ((:file "test-svg")
				     (:file "generate-brownian-trails")
				     (:file "new-brownian-tree")))))
