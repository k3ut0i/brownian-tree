(in-package :asdf-user)

(defsystem "brownian-tree"
  :version "1.0.0"
  :components ((:module "src"
                        :components ((:file "color")
				     (:file "svg"
                                            :depends-on ("svg/object"
							 "svg/polygon"
							 "svg/path"
							 "svg/ellipse"
							 "svg/circle"
							 "svg/text"
							 "svg/rect"))
                                     (:file "brownian-tree"
                                            :depends-on ("svg"))
                                     (:file "brownian-trails"
                                            :depends-on ("svg"))
                                     (:file "svg/object")
                                     (:file "svg/text"
                                            :depends-on ("svg/object"))
                                     (:file "svg/rect"
                                            :depends-on ("svg/object"))
                                     (:file "svg/polygon"
                                            :depends-on ("svg/object"))
                                     (:file "svg/path"
                                            :depends-on ("svg/object"))
                                     (:file "svg/ellipse"
                                            :depends-on ("svg/object"))
                                     (:file "svg/circle"
                                            :depends-on ("svg/object"))))))
