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
  (any (lambda (s)
	 (member c (bg s)))
       +all-colors+))

(defun any (f l)
  (if (f (car l))
      (car l)
      (any f (cdr l))))
