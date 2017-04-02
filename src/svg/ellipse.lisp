(defpackage :svg.ellipse
  (:use :cl
        :svg.object)
  (:export :ellipse))

(in-package :svg.ellipse)

(defclass ellipse (object)
  ((start-tag :accessor start-tag
              :initform "<ellipse")
   (end-tag :accessor end-tag
            :initform "/>")
   (points-tag :accessor points-tag)
   (center :accessor center
           :initarg :center)
   (rx :accessor rx
       :initarg :rx)
   (ry :accessor ry
       :initarg :ry)))

(defmethod initialize-instance :after ((obj ellipse) &rest args)
  (null args)
  (setf (points-tag obj)
        (format nil "cx=\"~A\" cy=\"~A\" rx=\"~A\" ry=\"~A\"" 
                (car (center obj))
                (cdr (center obj))
                (rx obj)
                (ry obj))))

(defmethod svg.object:draw ((obj ellipse))
  (reduce (lambda (a b) (format nil "~A ~A" a b))
          (mapcar (lambda (tag) (funcall tag obj))
                  '(start-tag stroke-tag fill-tag points-tag end-tag))))
