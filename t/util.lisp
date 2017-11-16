(defpackage :tutilities
  (:use :cl)
  (:export :file-equal-p)
  (:import-from :uiop :run-program))

(in-package :tutilities)

(defun file-equal-p (f1 f2)
  "Does the content of two files remain same?"
  (multiple-value-bind (out-res error-out-res ret-val)
      (run-program (list "/usr/bin/diff" f1 f2)
		   :ignore-error-status t)
    (declare  (ignore out-res error-out-res))
    (case ret-val
      (0 t)
      (1 nil))))
