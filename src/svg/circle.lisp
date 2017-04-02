(defpackage :svg.circle
  (:use :cl
        :svg.object)
  (:export :circle))

(in-package :svg.circle)

(defclass circle (object)
  ((start-tag :accessor start-tag
              :initform "<circle")
   (end-tag :accessor end-tag
            :initform "/>")
   (points-tag :accessor points-tag)
   (center :accessor center
           :initarg :center)
   (radius :accessor radius
           :initarg :radius))
  (:documentation "svg-cricle class"))

(defmethod initialize-instance :after((obj circle) &rest args)
  (null args)
  (setf (points-tag obj)
        (format nil "cx=\"~A\" cy=\"~A\" r=\"~A\""
                (car (center obj))
                (cdr (center obj))
                (radius obj))))

(defmethod svg.object:draw ((obj circle))
  (reduce (lambda (a b) (format nil "~A ~A" a b))
          (mapcar (lambda (tag) (funcall tag obj))
                  '(start-tag stroke-tag fill-tag points-tag end-tag))))
