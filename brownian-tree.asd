(in-package :asdf-user)

(defsystem "brownian-tree"
  :version "1.0.0"
  :components ((:module "src"
                        :components ((:file "svg")
                                     (:file "brownian-tree"
                                            :depends-on ("svg"))))))
