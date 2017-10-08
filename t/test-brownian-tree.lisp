(defpackage :test-brownian-tree
  (:use :cl
        :brownian-tree
        :svg
	:prove)
  (:import-from :uiop :run-program))
(in-package :test-brownian-tree)

(defvar +empty-files+  (list  "empty-brownian-tree-data.lisp"
			      "empty-netpbm-image.xpm"
			      "empty-svg-image.svg"))
(defvar +seeded-files+ (list "init-brownian-tree-data.lisp"
			     "init-netpbm-image.xpm"
			     "init-svg-image.svg"))

(defun file-equal-p (f1 f2)
  "Does the content of two files remain same?"
  (multiple-value-bind (out-res error-out-res ret-val)
      (run-program (list "/usr/bin/diff" f1 f2)
		   :ignore-error-status t)
    (declare  (ignore out-res error-out-res))
    (case ret-val
      (0 t)
      (1 nil))))

(defun test-empty-image ()
  "Test output process."
  (let* ((tree (make-instance 'brownian-tree))
	 (lisp-data-file "tmp-brownian-tree-data.lisp")
	 (netpbm-image-file "tmp-brownian-tree-image.xpm")
	 (svg-image-file "tmp-brownian-tree-svg-image.svg")
	 (tmp-files (list lisp-data-file
			  netpbm-image-file
			  svg-image-file)))
    (mapc #'(lambda (file format)
	      (draw-tree file format))
	  tmp-files
	  (list :lisp-data
		:netpbm-image
		:svg-image))
    (every #'file-equal-p
	   tmp-files
	   +empty-files+)
    (clean-up-files tmp-files)))

(defun test-seeded-image ()
  "Test output with a particular seed."
  (let* ((tree (make-instance 'brownian-tree))
	 (lisp-data-file "stmp-brownian-tree-data.lisp")
	 (netpbm-image-file "stmp-brownian-tree-image.xpm")
	 (svg-image-file "stmp-brownian-tree-svg-image.svg")
	 (tmp-files (list lisp-data-file
			  netpbm-image-file
			  svg-image-file)))
    (seed-tree tree)
    (mapc #'draw-tree)))

(plan 2)
(subtest "empty images"
  (test-empty-image))
(subtest "seeded images"
  (test-seeded-image))
(finalize)
