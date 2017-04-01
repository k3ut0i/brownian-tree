(defpackage :svg.object
  (:use cl)
  (:export :object
           :stroke-tag
           :fill-tag
           :uid
           :draw))

(in-package :svg.object)

; TODO: complete with all parameters from SVG definition.
(defclass object ()
  ((uid :accessor uid
        :initarg :uid
        :initform nil)
   (stroke-color :accessor stroke-color ;*stroke* in when object is line-drawn
                 :initarg :stroke-color
                 :initform "black")
   (fill-color :accessor fill-color
               :initarg :fill-color
               :initform "white")
   (stroke-tag :accessor stroke-tag)
   (fill-tag :accessor fill-tag)))

(defmethod initialize-instance :after ((o object) &rest args)
  (null args)
  (setf (stroke-tag o) (format nil "stroke=\"~A\"" (stroke-color o)))
  (setf (fill-tag o) (format nil "fill=\"~A\"" (fill-color o))))

(defgeneric draw (object)
  (:documentation "return svg-string for any type of svg object"))

;; write a method to draw generic object rather than specialized one like in svg.polygon
;; using tags and extra attributes in their class definition.
