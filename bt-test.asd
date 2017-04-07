(in-package :cl-user)

(defpackage :bt-test-asd
  (:use :cl
        :asdf))

(in-package :bt-test-asd)

(defsystem "bt-test"
  :depends-on (:prove
               :brownian-tree)
  :components ((:module "t"
                        :components ((:file "new-brownian-tree")
                                     (:file "test-svg")
                                     (:file "validate-svg")))))