(in-package :cl-user)
(defpackage :gen-btrails
  (:use :cl
        :svg
        :brownian-trails)
  (:export :generate-btrails-image))

(in-package :gen-btrails)

(defun generate-btrails-image (&key size num-trails initial-seed filename bg-color)
  (let ((bt (create-btrails :initial-seed initial-seed
			    :num-trails num-trails
			    :bg-color bg-color
			    :size size)))
    (draw-brownian-trails bt filename)))
