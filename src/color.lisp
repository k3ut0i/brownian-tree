(defpackage :color
  (:use :cl)
  (:export :get-color-set))
(in-package :color)

(defparameter +all-colors+ '()
  "set of all defined color-set's")

;; a color set with a few background colors and color which
;; contrast higly with background and a few which look smooth on the background.
(defclass color-set ()
  ((bg :accessor bg)
   (high-contrast :accessor high-contrast)
   (smooth :accessor smooth)))

(defun get-color-set (c)
  "a color-set if it exists with C as background color"
  (some (lambda (s)
	 (member c (bg s)))
       +all-colors+))

(defun color-gradient (start end)
  "Color gradient line from START to END in RGB color space."
  (declare (type (simple-vector 3) start end))
  (let ((s0 (svref start 0))
	(s1 (svref start 1))
	(s2 (svref start 2))
	(e0 (svref end 0))
	(e1 (svref end 1))
	(e2 (svref end 2)))
    (labels ((affine-line (a0 a1 a)
	       (+ a0 (* (- a1 a0) a))))
      (lambda (a)
	(vector (affine-line s0 e0 a)
		(affine-line s1 e1 a)
		(affine-line s2 e2 a))))))
