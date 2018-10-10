(defsystem "svg"
  :version "0.1.0"  
  :components ((:module "src"
			:serial t
			:components ((:file "svg")
				     (:file "svg/object")
				     (:file "svg/text")
				     (:file "svg/circle")
				     (:file "svg/ellipse")
				     (:file "svg/rect")
				     (:file "svg/polygon")
				     (:file "svg/path")
				     (:file "color")))))
